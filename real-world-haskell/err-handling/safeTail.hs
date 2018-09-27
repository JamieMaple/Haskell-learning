main :: IO ()
main = putStrLn "Not implemented"

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

