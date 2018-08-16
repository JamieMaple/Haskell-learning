-- hof
multTree :: Int -> Int -> Int -> Int
multTree x y z = x * y * z
multWithHundred = multTree 100
-- curry/section
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)
-- curry/elem
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- hof/function param
applyTwice :: (a -> a) -> a -> a
applyTwice f a = f (f a)

-- zipWith
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- flip
flip' :: (a -> b -> c) -> b -> a -> c 
flip' f y x = f x y

-- map
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map f xs

-- filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x       = x:filter' f xs
    | otherwise = filter' f xs

-- collatz sequence
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | odd n  = n:chain (n*3 + 1)
    | even n = n:chain (n `div` 2)

