import Control.Monad.State
import System.Random

getRandom :: Random a => State StdGen a
getRandom = do
    gen <- get
    let (val, newGen) = random gen
    put newGen
    return val


