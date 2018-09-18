import Control.Monad
import System.FilePath ((</>))
import Control.Monad.Writer (WriterT, tell)
import Control.Monad.Trans (liftIO)
import System.Directory (doesDirectoryExist, getDirectoryContents)

listDirectory :: FilePath -> IO [FilePath]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /=  ".."

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
    contents <- listDirectory path
    rest <- forM contents $ \name -> do
                let newName = path </> name
                isDir <- doesDirectoryExist newName
                if isDir
                    then countEntriesTrad newName
                    else return []
    return $ (path, length contents) : concat rest

countEntriesTrad' :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntriesTrad' path = do
    contents <- liftIO . listDirectory $ path
    tell [(path, length contents)]
    forM_ contents $ \name -> do
        let newName = path </> name
        isDir <- liftIO . doesDirectoryExist $ newName
        when isDir $ countEntriesTrad' newName

main :: IO ()
main = putStrLn "not implemented"
