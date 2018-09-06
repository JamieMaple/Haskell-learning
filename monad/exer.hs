import Data.Monoid
import Data.Semigroup
import Control.Monad
import Control.Monad.Writer

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left+n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right) 
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

--x -: f = f x

foo :: Maybe String
foo = do
    x <- Just 321
    y <- Just "!"
    Just (show x ++ y)

routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    Nothing -- banana
    second <- landRight 1 first
    landLeft 1 second

wopwop :: Maybe Char
wopwop = do
    (x:xs) <- Just ""
    return x

--[1,2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch)

listOfTuples :: [(Int, Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a', 'b']
    return (n, ch)

--[(n,ch) | n <- [1,2], ch <- ['a', 'b']]

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c', r') <- [(c+2,r+1), (c+2,r-1), (c-2,r+1), (c-2, r-1)
        , (c+1,r+2), (c+1,r-2), (c-1,r+2) , (c-1,r-2)
        ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

reachIn3Pos :: KnightPos -> KnightPos -> [[KnightPos]]
reachIn3Pos start end = do
    first <- moveKnight start
    second <- moveKnight first
    third <- moveKnight second
    guard (third == end)
    return [start, first, second, end]

--(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
--f <=< g = (\x -> g x >>= f)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number:" ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)

gcd' :: Int -> Int -> Int
gcd' a b
    | b == 0 = a
    | otherwise = gcd' b (a `mod` b)

gcd'' :: Int -> Int -> Writer [String] Int
gcd'' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd'' b (a `mod` b)

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

-- for original book, ghci 8.4 will get a problem
instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  --(DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))

gcdReverse :: Int -> Int -> Writer (DiffList String) Int 
gcdReverse a b
    | b == 0 = do
        tell (toDiffList ["Finished with " ++ show a])
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result

-- faster one
-- fromDiffList . snd . runWriter . finalCountDown $ 500000
finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])

-- slower one
-- very slow one ...
-- snd . runWriter . finalCountDown' $ 500000
finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do
    tell ["0"]
finalCountDown' x = do
    finalCountDown' (x-1)
    tell [show x]

foo' :: Writer String Int
foo' = do
    tell "!"
    a <- writer (1, "Hello")
    tell "World"
    return a
-- !HelloWorld

