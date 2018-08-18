main = do
    putStrLn "Hello, What your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
