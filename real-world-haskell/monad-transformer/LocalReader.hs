module LocalReader where

import Control.Monad.Reader

myName :: String -> Reader String String
myName step = do
    name <- ask
    return (step ++ ", I am " ++ name)

testReader :: Reader String String
testReader = do
    name <- ask
    return (name ++ " test !")

localExample :: Reader String (String, String, String)
localExample = do
    a <- myName "first"
    b <- local (++"dy") (myName "Second")
    c <- myName "Third"
    return (a, b, c)

