main = interact respondPalindromes

shortLineOnly :: String -> String
shortLineOnly = unlines . filter (\line -> length line < 10) . lines

respondPalindromes :: String -> String
respondPalindromes =
    unlines .
    map (\xs -> if isPal xs then "panlindrome" else "not a palindrome") .
    lines

isPal :: String -> Bool
isPal xs = xs == reverse xs

