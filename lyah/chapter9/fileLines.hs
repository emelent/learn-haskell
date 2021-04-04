import System.Environment
import System.IO
import System.Directory
import System.IO.Error
import Control.Exception


fileLiner :: IO ()
fileLiner = do
    (fileName:_) <- getArgs
    fileExists <- doesFileExist fileName
    if fileExists
        then do contents <- readFile fileName
                putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
        else do putStrLn "The file doesn't exist!"


handler :: IOError -> IO ()
handler e = putStrLn ("Failed to read file: " ++ (ioeGetErrorString e))
--    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
--    | isPermissionError e = putStrLn "Sorry, you don't have the permission to read that file."
--    | otherwise = putStrLn (ioeGetErrorString) -- "Whoops, had some trouble!"


toTry :: IO ()
toTry = do
    (fileName: _) <- getArgs
    content <- readFile fileName
    putStrLn $ "The file has " ++ (show (length (lines content))) ++ " lines!"




main = toTry `catch` handler
