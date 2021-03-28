import Control.Monad
import Data.Char

-- main = putStrLn "hello, world"

-- main = do
--     putStrLn "Hello, what's your name?"
--     name <- getLine
--     putStrLn ("Hey " ++ name ++ ", you rock!")

someFunc :: IO String
someFunc = do
    return "Hahaha"

readLine = do   
    line <- getLine  
    msg <- someFunc
    putStrLn msg
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            readLine 
    putStrLn "and we're out"
  
upperCaseReadLine = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l  

reverseWords :: String -> String  
reverseWords = unwords . map reverse . words  


-- only processes input after pressing enter and not on key stroke
readSpace = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            main  
        else return ()  


readSpace' = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        readSpace'

capsLocked = forever $ do
    putStr "Give me some input: "
    input <- getLine
    putStrLn $ map toUpper  input

usingForM = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        getLine)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    forM colors putStrLn 

usingMapM = do   
    colors <- mapM  (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        getLine)  [1,2,3,4]
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors  

main = do 
    usingForM 
    return ()