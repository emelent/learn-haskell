import System.Environment
import Data.List
import Data.Char (isDigit)

-- main = do 
--     args <- getArgs 
--     progName <- getProgName 
--     putStrLn "The arguments are:"
--     mapM_ putStrLn args
--     putStrLn "The program name is:"
--     putStrLn progName
    

isNumeric:: [Char] -> Bool
isNumeric = foldl (\acc x -> if not acc then acc else isDigit x) True 


add::[String] -> IO ()
add [file, item] = do 
    appendFile file (item ++ "\n")
    -- content <- readFile file 
    -- let 
    --     numberedContent = zipWith (\n line -> show n ++ " - " ++ line) [1..] (lines content)
    -- putStrLn "TODO: "
    -- putStrLn $ unlines numberedContent

view::[String] -> IO ()
view [file] = do 
    content <- readFile file 
    putStrLn "TODO: "
    let numberedContent = zipWith (\n line -> show n ++ " - " ++ line) [1..] (lines content)
    putStrLn $ unlines numberedContent
    return ()

remove::[String] -> IO ()
remove [file, numberStr]= do 
    content <- readFile file 
    let todoItems = lines content
        number = read numberStr :: Int
    
    if number > length todoItems
        then return ()
        else (do
            let target = todoItems !! (number - 1)
                filtered = filter (\x -> x /= target) todoItems
                lineContent = zipWith (\n line -> show n ++ " " ++ line) [1..] filtered
            writeFile file $ unlines filtered
        )

dispatch::[(String, [String] -> IO ())]
dispatch = [ ("add", add)
    , ("view", view)
    , ("remove", remove)
    ]

main = do
   (command:args) <- getArgs 
   let (Just action) = lookup command dispatch
   action args
     