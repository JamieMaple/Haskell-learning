import System.Environment
import System.IO
import System.Directory
import Data.List

{-
 -dispatch :: String -> [String] -> IO ()
 -dispatch "view" argList   = view
 -dispatch "add" argList    = add
 -dispatch "remove" argList = remove
 -}

dispatch :: [(String,[String] -> IO ())]
dispatch = [("add",add)
           ,("view",view)
           ,("remove",remove)
           ,("bump",bump)
           ]

add :: [String] -> IO ()
add [fileName, todoItem] =
    appendFile fileName $ todoItem ++ "\n"

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] $ lines contents
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName,tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number       = read numberString
        todoItems    = lines contents
        newTodoItems = delete (todoItems !! number) todoItems
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName 

bump :: [String] -> IO ()
bump [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let number = read numberString
        todoItems = lines contents
        topItem = todoItems !! number
        newTodoItems = topItem : (delete topItem todoItems)
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName

main = do
    (command:argList) <- getArgs
    let (Just action) = lookup command dispatch
    action argList

