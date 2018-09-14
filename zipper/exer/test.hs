joinList :: [[a]] -> [a]
joinList [] = []
joinList (xs:xss) = xs ++ joinList xss

intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse s (x:[]) = x
intersperse s (x:xs) = x ++ s:intersperse s xs

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

getHeight :: Tree a -> Int
getHeight Empty = 0
getHeight (Node _ l r) = 1 + (max (getHeight l) (getHeight r))

testTree = Node 1 Empty Empty
testTree' = Node 1 (Node 2 Empty Empty) Empty


