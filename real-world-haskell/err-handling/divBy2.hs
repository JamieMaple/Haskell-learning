{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Error

main :: IO ()
main = putStrLn "Not implemented"

divBy :: Integral a => a -> [a] -> Either String [a]
divBy _ [] = Right []
divBy _ (0:_) = Left "divBy: division by 0"
dibBy numberator (denom:xs) = 
    case divBy numberator xs of
        Left x -> Left x
        Right result -> Right ((numberator `div` denom):result)

data DivByError a = DivBy0
                  | ForbiddenDenominator a
                  | OtherDivByError String
                  deriving (Eq, Read, Show)

instance Error (DivByError a) where
    strMsg x = OtherDivByError x


