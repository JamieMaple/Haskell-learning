reverseWords :: String -> String
reverseWords = unwords . map reverse . words

putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do
    putChar x
    putStr xs

main = do
    line <- getLine
    if null line
       then return()
       else do
           putStrLn $ reverseWords line
           main
