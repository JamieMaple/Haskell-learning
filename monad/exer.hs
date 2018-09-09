import Data.List
import Data.Ratio
import Data.Monoid
import System.Random
import Control.Monad
import Data.Semigroup
import Control.Monad.State
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

addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a+b)

type Stack = [Int]

-- Control.Monad.Stack
getStack :: State Stack Stack
getStack = state (\s -> (s, s))

putStack :: Stack -> State Stack ()
putStack newState = state $ \s -> ((), newState)

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    a <- pop
    pop

stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a, b, c)

getBirdsInfo :: Pole -> String
getBirdsInfo (left, right) = "Birds: left " ++ show left ++ ", right " ++ show right

landLeft' :: Birds -> Pole -> Either String Pole
landLeft' n (left, right)
    | abs ((left + n) - right) < 4 = Right (left + n, right)
    | otherwise = Left $ getBirdsInfo (left + n, right)

landRight' :: Int -> Pole -> Either String Pole
landRight' n (left, right)
    | abs (left - (right + n)) < 4 = Right (left, right + n)
    | otherwise = Left $ getBirdsInfo (left, right + n)

routine' :: Either String Pole
routine' = do
    start <- return (0, 0)
    first <- landLeft' 1 start
    second <- landRight' 3 first
    landLeft' 10 second

keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping" ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9 = Nothing
    | otherwise = Just (acc + x)

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)
    return result

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x
                                _ -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((y*x):ys)
foldingFunction (x:y:ys) "+" = return ((y+x):ys)
foldingFunction (x:y:ys) "-" = return ((y-x):ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

-- param steps for knight movement
inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs, p) = map (\(x, r) -> (x, p*r)) innerxs

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x, p)) xs

instance Applicative Prob where
    pure = return
    (Prob [(f, _)]) <*> something = fmap f something

instance Monad Prob where
    return x = Prob [(x,1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

thisSituation :: Prob (Prob Char)
thisSituation = Prob
    [(Prob [('a', 1%2), ('b', 1%2)], 1%4)
    ,(Prob [('c', 1%2), ('d', 1%2)], 3%4)
    ]

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1%2), (Tails, 1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1%10), (Tails, 9%10)]

flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (==Tails) [a,b,c])

sumCoinFoldingFunc :: [(Bool, Rational)] -> (Bool, Rational) -> [(Bool, Rational)]
sumCoinFoldingFunc [] (False, r) = [(False, r), (True, 0)]
sumCoinFoldingFunc [] (True, r)  = [(False, 0), (True, r)]
sumCoinFoldingFunc (xs:ys:[]) x  = case x of (False, r) -> [(False, r + (snd xs)), ys]
                                             (True, r)  -> [xs, (True, r + (snd ys))]

sumCoin :: Prob Bool -> Prob Bool
sumCoin (Prob xs) = Prob $ foldl sumCoinFoldingFunc [] xs

