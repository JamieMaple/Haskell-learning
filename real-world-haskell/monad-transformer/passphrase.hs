module Passphrase where

import Data.Char (isAlpha, isNumber, isPunctuation)

getPassphrase :: IO (Maybe String)
getPassphrase = do s <- getLine
                   if isValid s then return $ Just s
                                else return Nothing

isValid :: String -> Bool
isValid s = length s >= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s

askPassphrase :: IO()
askPassphrase = do putStrLn "Insert your new passphrase:"
                   maybe_value <- getPassphrase
                   case maybe_value of
                    Just value -> do putStrLn "Storing in database..."
                    Nothing -> putStrLn "Passphrase invalid."

main :: IO ()
main = askPassphrase

