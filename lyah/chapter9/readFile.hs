import System.IO
import Data.Char

-- main = do
--     handle <- openFile "haiku.txt" ReadMode
--     contents <- hGetContents handle
--     putStr contents
--     hClose handle

-- main = do
--   withFile"haiku.txt"ReadMode( \handle -> do
--         contents <- hGetContents handle
--         putStr contents
--     )

readAFileOfSorts = do
    contents <- readFile "haiku.txt"
    putStr contents

readAndWrite = do
    contents <- readFile "haiku.txt"
    writeFile "haiku-caps.txt" $ map toUpper contents


appendToFile = do     
    todoItem <- getLine  
    appendFile "todo.txt" (todoItem ++ "\n")  -- add \n because we don't get one from getLine

main = appendToFile 
