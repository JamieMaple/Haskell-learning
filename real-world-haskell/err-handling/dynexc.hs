{-# LANGUAGE DeriveDataTypeable #-}

module Dynexc where

import Data.Dynamic
import Control.Exception

data SqlError = SqlError {
    setState :: String,
    setNativeError :: Int,
    setErrorMsg :: String
} deriving (Eq, Show, Read, Typeable)

instance Exception SqlError

{- | Execute the given IO action.
 - if it raises a 'SqlError' then execute the supplied handler
 - and return its return value.
 - Otherwise, proceed as normal
 -}
catchSql :: IO a -> (SqlError -> IO a) -> IO a
catchSql = catch

{- | Like 'catchSql', with the order of arguments reversed
 -}
handleSql :: (SqlError -> IO a) -> IO a -> IO a
handleSql = flip catch

{- | Catches 'SqlError's, and re0raises them as IO errors with fail
 - Useful if you dont care to catch SQL errors, but want to see a sane
 - error message if one happens. One would often use this as high-level
 - wrapper around SQL calls
 -}
handleSqlError :: IO a -> IO a
handleSqlError action =
    catchSql action handler
    where handler e = fail ("SQL error: " ++ show e)

throwSqlError :: String -> Int -> String -> a
throwSqlError state nativeerror errormsg =
    throw (SqlError state nativeerror errormsg)

throwSqlErrorIO :: String -> Int -> String -> IO a
throwSqlErrorIO state nativeerror errormsg = evaluate (throwSqlError state nativeerror errormsg)

