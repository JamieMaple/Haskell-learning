solveDouble :: String -> Double
solveDouble = head . foldl foldingFunc [] . words
    where foldingFunc (x:y:xs) "*"    = y * x : xs
          foldingFunc (x:y:xs) "+"    = y + x : xs
          foldingFunc (x:y:xs) "-"    = y - x : xs
          foldingFunc (x:y:xs) "/"    = y / x : xs
          foldingFunc (x:xs) "log"    = log x : xs
          foldingFunc xs "sum"        = [sum xs]
          foldingFunc xs numberString = read numberString : xs



