module UglyStack where

import System.Directory
import System.FilePath
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

data AppConfig = AppConfig {
    cfgMaxDepth :: Int
} deriving Show

data AppState = AppState {
    stDeepestReached :: Int
} deriving Show

type App = ReaderT AppConfig (StateT AppState IO)
-- App2 is not allowed to partially apply a type synonym(部分应用)
-- but we can use that in `newtype`
-- type App2 a = ReaderT AppConfig (StateT AppState IO) a

runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 3
    in runStateT (runReaderT k config) state


constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
    contents <- liftIO .  listDirectory $ path
    cfg <- ask
    rest <- forM contents $ \name -> do
                let newPath = path </> name
                isDir <- liftIO $ doesDirectoryExist newPath
                if isDir && curDepth < cfgMaxDepth cfg
                    then do
                        let newDepth = curDepth + 1
                        st <- get
                        when (stDeepestReached st < newDepth) $
                            put st { stDeepestReached = newDepth } -- put AppState { stDeepestReached = 1 }
                        constrainedCount newDepth newPath
                    else return []
    return $ (path, length contents) : concat rest

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype MyApp a = MyA {
    runA :: ReaderT AppConfig (StateT AppState IO) a }
    --deriving (Monad, MonadIO, MonadReader AppConfig, MonadState AppState)

implicitGet :: App AppState
implicitGet = get

type Foo = StateT Int (State String)

outerPut :: Int -> Foo ()
outerPut = put

innerPut :: String -> Foo ()
innerPut = lift . put

type Bar = ReaderT Bool Foo

barPut :: String -> Bar ()
barPut = lift . lift . put

-- 显式
explicitGet :: App AppState
explicitGet = lift get

runMyApp :: MyApp a -> Int -> IO (a, AppState)
runMyApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 0
    in runStateT (runReaderT (runA k) config) state


