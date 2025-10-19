import Control.Exception (IOException, try)
import Control.Monad (forM, forM_)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString qualified as B
import Data.List (minimumBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Time.Clock (UTCTime)
import System.Directory (doesFileExist, getModificationTime, listDirectory, removeFile)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeFileName, (</>))
import System.IO (hPutStrLn, stderr)

-- 为了代码清晰，定义类型别名
type FileHash = B.ByteString
type FileInfo = (UTCTime, FilePath)

-- 程序主入口
main :: IO ()
main = do
    -- 获取命令行参数
    args <- getArgs
    case args of
        [path] -> processDirectory path
        _ -> do
            -- 如果参数不正确，打印用法并退出
            hPutStrLn stderr "用法: dff <文件夹路径>"
            exitFailure

-- 处理指定目录的函数
processDirectory :: FilePath -> IO ()
processDirectory path = do
    putStrLn $ "正在扫描文件夹: " ++ path
    -- 1. 递归获取所有文件的路径
    allFiles <- getRecursiveContents path
    putStrLn $ "找到 " ++ show (length allFiles) ++ " 个文件。"

    -- 2. 为每个文件计算哈希值，并处理可能发生的读取错误
    hashedFiles <- catMaybes <$> mapM hashFileSafe allFiles
    putStrLn "文件哈希计算完成。"

    -- 3. 根据哈希值将文件分组
    let fileGroups = groupFilesByHash hashedFiles
    -- 筛选出包含多个文件（即重复文件）的组
    let duplicateGroups = Map.filter (\files -> length files > 1) fileGroups

    if Map.null duplicateGroups
        then putStrLn "未找到重复文件。"
        else do
            putStrLn $ "找到 " ++ show (Map.size duplicateGroups) ++ " 组重复文件。正在处理..."
            -- 4. 遍历并处理每一组重复文件
            forM_ (Map.elems duplicateGroups) processDuplicateGroup
            putStrLn "清理完成。"

-- 递归地获取目录下的所有文件路径
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topPath = do
    -- 列出当前目录下的所有条目（文件和子目录）
    names <- listDirectory topPath
    paths <- forM names $ \name -> do
        let path = topPath </> name
        isFile <- doesFileExist path
        if isFile
            then return [path]
            else getRecursiveContents path -- 如果是目录，则递归调用
    return (concat paths) -- 将结果合并成一个列表

-- 安全地计算文件哈希值，会处理IO异常
hashFileSafe :: FilePath -> IO (Maybe (FilePath, FileHash))
hashFileSafe path = do
    -- 尝试使用严格的readFile来读取文件内容，这可以防止文件句柄被耗尽
    result <- try (B.readFile path) :: IO (Either IOException B.ByteString)
    case result of
        Left e -> do
            -- 如果读取失败，打印警告并返回Nothing
            hPutStrLn stderr $ "警告: 无法读取文件 " ++ path ++ " (" ++ show e ++ ")"
            return Nothing
        Right content -> do
            -- 如果成功，计算哈希值并返回
            let filehash = hash content
            return $ Just (path, filehash)

-- 将 (文件路径, 文件哈希) 列表转换成一个Map，其中键是哈希，值是文件路径列表
groupFilesByHash :: [(FilePath, FileHash)] -> Map.Map FileHash [FilePath]
groupFilesByHash = foldr (\(path, fileHash) acc -> Map.insertWith (++) fileHash [path] acc) Map.empty

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
