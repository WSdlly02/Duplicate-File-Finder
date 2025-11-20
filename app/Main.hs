-- 主模块
module Main where

-- 导入所需的库

import System.Directory (doesFileExist, getFileSize, getModificationTime, listDirectory, removeFile)
import System.Environment (getArgs)
import System.FilePath (takeFileName, (</>))

-- 导入更多 System.IO 函数

import Control.Exception (IOException, try)
import Control.Monad (forM, forM_)
import System.Exit (exitFailure)
import System.IO (BufferMode (BlockBuffering), Handle, IOMode (ReadMode), hClose, hPutStrLn, hSetBuffering, openFile, stderr)

-- 切换回 严格 (Strict) ByteString

import Crypto.Hash (Digest, SHA256)
import qualified Data.ByteString as B

-- 导入手动哈希上下文函数
import Crypto.Hash (Context, hashFinalize, hashInit, hashUpdate)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime)

-- 从Data.List导入minimumBy和严格的foldl'
import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import System.IO.Unsafe (unsafeInterleaveIO)

-- 为了代码清晰，定义类型别名
type FileHash = Digest SHA256
type FileInfo = (UTCTime, FilePath)
type FileSize = Integer

-- 新增：高效的、命令式的流式哈希函数
-- 这模拟了Python的C语言优化循环
hashFileStreaming :: FilePath -> IO FileHash
hashFileStreaming path = do
    -- 1. 打开文件句柄
    h <- openFile path ReadMode
    -- 设置更大的缓冲区 (1MB)
    hSetBuffering h (BlockBuffering (Just 1048576))
    -- 2. 初始化哈希上下文
    let context = hashInit :: Context SHA256
    -- 3. 调用循环体
    finalContext <- loop h context
    -- 4. 关闭句柄
    hClose h
    -- 5. 返回最终哈希值
    return $ hashFinalize finalContext
  where
    -- 定义循环体 - 使用1MB块
    loop :: Handle -> Context SHA256 -> IO (Context SHA256)
    loop h ctx = do
        -- 以1MB的块读取文件 (更大的块，减少系统调用)
        chunk <- B.hGet h 1048576
        if B.null chunk
            then return ctx -- 文件结束，返回最终的上下文
            else do
                -- 更新哈希上下文
                let !newCtx = hashUpdate ctx chunk
                -- 继续循环
                loop h newCtx

-- 程序主入口
main :: IO ()
main = do
    -- 获取命令行参数
    args <- getArgs
    case args of
        [path] -> processDirectory path
        _ -> do
            -- 如果参数不正确，打印用法并退出
            hPutStrLn stderr "用法: find-dups <文件夹路径>"
            exitFailure

-- 处理指定目录的函数
processDirectory :: FilePath -> IO ()
processDirectory path = do
    -- 1. 递归获取所有文件的路径 (使用惰性IO)
    allFiles <- getRecursiveContents path

    -- 2. ★★★ 关键优化：先按文件大小分组 ★★★
    -- 只对大小相同的文件计算哈希值
    filesWithSizes <- catMaybes <$> mapM getFileSizeSafe allFiles
    let sizeGroups = groupFilesBySize filesWithSizes
    -- 只处理有多个文件的大小组
    let potentialDuplicates = concat $ Map.elems $ Map.filter (\files -> length files > 1) sizeGroups

    -- 3. 对可能重复的文件计算哈希值
    hashedFiles <- catMaybes <$> mapM hashFileSafe potentialDuplicates

    -- 4. 根据哈希值将文件分组
    let fileGroups = groupFilesByHash hashedFiles
    -- 筛选出包含多个文件（即重复文件）的组
    let duplicateGroups = Map.filter (\files -> length files > 1) fileGroups

    if Map.null duplicateGroups
        then putStrLn "未找到重复文件。"
        else do
            putStrLn $ "找到 " ++ show (Map.size duplicateGroups) ++ " 组重复文件。正在处理..."
            -- 5. 遍历并处理每一组重复文件
            forM_ (Map.elems duplicateGroups) processDuplicateGroup
            putStrLn "清理完成。"

-- 递归地获取目录下的所有文件路径 (使用惰性IO优化)
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topPath = do
    -- 列出当前目录下的所有条目（文件和子目录）
    names <- try (listDirectory topPath) :: IO (Either IOException [String])
    case names of
        Left _ -> return []
        Right n -> do
            paths <- unsafeInterleaveIO $ forM n $ \name -> unsafeInterleaveIO $ do
                let path = topPath </> name
                -- 简单的异常处理，防止符号链接等问题
                isFile <- doesFileExist path
                if isFile
                    then return [path]
                    else getRecursiveContents path -- 如果是目录，则递归调用
            return (concat paths) -- 将结果合并成一个列表

-- 安全地获取文件大小
getFileSizeSafe :: FilePath -> IO (Maybe (FilePath, FileSize))
getFileSizeSafe path = do
    result <- try (getFileSize path) :: IO (Either IOException Integer)
    case result of
        Left _ -> return Nothing
        Right size -> return $ Just (path, size)

-- 将 (文件路径, 文件大小) 列表转换成Map，按大小分组
groupFilesBySize :: [(FilePath, FileSize)] -> Map.Map FileSize [FilePath]
groupFilesBySize = foldl' (\acc (path, size) -> Map.insertWith (++) size [path] acc) Map.empty

-- 安全地计算文件哈希值，会处理IO异常
hashFileSafe :: FilePath -> IO (Maybe (FilePath, FileHash))
hashFileSafe path = do
    -- ★ 尝试使用我们新的、高效的流式哈希函数
    result <- try (hashFileStreaming path) :: IO (Either IOException FileHash)
    case result of
        Left _ -> return Nothing
        Right filehash -> return $ Just (path, filehash)

-- 将 (文件路径, 文件哈希) 列表转换成一个Map，其中键是哈希，值是文件路径列表
groupFilesByHash :: [(FilePath, FileHash)] -> Map.Map FileHash [FilePath]
-- ★★★ 使用严格的 foldl' ★★★
groupFilesByHash = foldl' (\acc (path, hash) -> Map.insertWith (++) hash [path] acc) Map.empty

-- 处理一组重复的文件：找到最老的一个，然后删除其余的
processDuplicateGroup :: [FilePath] -> IO ()
processDuplicateGroup duplicates = do
    -- 获取每个文件的修改时间
    filesWithTimes <- mapM getFileTime duplicates

    -- 使用 comparing 函数找到修改时间最老的文件
    let oldestFile = minimumBy (comparing fst) filesWithTimes
    let (oldestTime, oldestPath) = oldestFile

    putStrLn $ "\n发现一组重复文件 (" ++ takeFileName oldestPath ++ "):"
    putStrLn $ "  - [保留] " ++ oldestPath ++ " (最早修改时间: " ++ show oldestTime ++ ")"

    -- 筛选出需要删除的较新文件
    let filesToDelete = filter (\(_, path) -> path /= oldestPath) filesWithTimes
    forM_ filesToDelete $ \(_, path) -> do
        -- 尝试删除文件并处理可能发生的IO异常
        result <- try (removeFile path) :: IO (Either IOException ())
        case result of
            Left e -> hPutStrLn stderr $ "  - [错误] 无法删除 " ++ path ++ " (" ++ show e ++ ")"
            Right () -> putStrLn $ "  - [删除] " ++ path

-- 获取单个文件的修改时间
getFileTime :: FilePath -> IO FileInfo
getFileTime path = do
    time <- getModificationTime path
    return (time, path)