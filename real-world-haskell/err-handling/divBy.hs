main :: IO ()
main = putStrLn "Not implemented"


divBy :: Integral a => a -> [a] -> Maybe [a]
divBy _ [] = Just []
divBy _ (0:_) = Nothing
divBy numberator (x:xs) =
    case divBy numberator xs of
        Nothing -> Nothing
        Just results -> Just ((numberator `div` x):results)

divBy' :: Integral a => a -> [a] -> [Maybe a]
divBy' numberator denominators =
    map worker denominators
    where worker 0 = Nothing
          worker x = Just (numberator `div` x)

--default use IO Monad
divByGeneric :: (Monad m, Integral a) => a -> [a] -> m [a]
divByGeneric _ [] = return []
divByGeneric _ (0:_) = fail "division by zero in div ByGeneric"
divByGeneric numberator (denom:xs) = do
        next <- divByGeneric numberator xs
        return ((numberator `div` denom) : next)


