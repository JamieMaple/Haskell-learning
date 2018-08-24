import Data.List
import System.Random

data Section    = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
data Label      = A | B | C deriving (Show)
type Path       = [(Label, Int)]
type RoadSystem = [Section]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem
     in if sum (map snd bestAPath) <= sum (map snd bestBPath)
            then bestAPath
            else bestBPath

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let timeA = sum (map snd pathA)
        timeB = sum (map snd pathB)
        forwardTimeToA = timeA + a
        crosstimeToA = timeB + b + c
        forwardTimeToB = timeB + b
        crosstimeToB = timeB + a + c
        newPathToA = if forwardTimeToA < crosstimeToA
                        then (A, a):pathA
                        else (C, c):(B, b):pathB
        newPathToB = if forwardTimeToB < crosstimeToB
                        then (B, b):pathB
                        else (C, c):(A, a):pathA
    in (newPathToA, newPathToB)

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

getRandoms :: Int -> StdGen -> [Int]
getRandoms n gen =
    let rands = randomRs (5, 100) gen
    in take n rands

main = do
    --contents <- getContents
    randoms <- fmap (getRandoms 24) getStdGen
    let threes = groupsOf 3 randoms
        roadSystem = map (\[a,b,c] -> Section a b c) threes 
        path = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathTime = sum $ map snd path
    putStrLn $ "The data is " ++ show roadSystem
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "Time taken: " ++ show pathTime

