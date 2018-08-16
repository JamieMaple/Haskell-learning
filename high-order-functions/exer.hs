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

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..1000]))
    where isLong xs = length xs > 15

sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' = foldl (+) 0

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (flip(\acc x -> [f x] ++ acc)) [] xs
-- map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs
-- 左折叠往后添加元素效率比较低

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldr (\x acc -> if x == y then True else acc) False ys

-- foldable function implements
reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

-- maximum
maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []


last' :: [a] -> a
last' = foldl1 (\_ x -> x)


and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

-- (&&) :: Bool -> Bool -> Bool
-- True && x = x
-- False && _ = False

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

