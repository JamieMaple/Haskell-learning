import System.IO
import System.Directory
import Control.Exception
import Data.List

main = do
    handle <- openFile "todo.txt" ReadMode
    contents <- hGetContents handle
    let todoItems = lines contents
        numberedTasked = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoItems
    putStrLn "There are your todo-itmes:"
    putStr $ unlines numberedTasked
    putStrLn "Which one do you want to delete"
    numberString <- getLine
    let number = read numberString
        newTodoItems = delete (todoItems !! number) todoItems
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle $ unlines newTodoItems
            hClose tempHandle
            hClose handle
            removeFile "todo.txt"
            renameFile tempName "todo.txt")
