import System.Random

main = do
    gen <- getStdGen
    putStr $ take 10 $ randomRs ('a', 'z') gen
