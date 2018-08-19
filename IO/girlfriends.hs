import System.IO
import Control.Exception

{-
 -main = do
 -    handle <- openFile "girlfriends.txt" ReadMode
 -    contents <- hGetContents handle
 -    putStr contents
 -    hClose handle
 -}

{-
 -main = do
 -    withFile "girlfriends.txt" ReadMode (\handle -> do
 -        contents <- hGetContents handle
 -        putStr contents)
 -}

main = do
    contents <- readFile "girlfriends.txt"
    putStr contents


withFile' :: FileType -> IOMode -> (Handle -> IO a) -> IO a
withFile' file mode f = bracket (openFile file mode)
    (\handle -> hClose handle)
    (\handle -> f handle)
