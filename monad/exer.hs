import Control.Monad

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

