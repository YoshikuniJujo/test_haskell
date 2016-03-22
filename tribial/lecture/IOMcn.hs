module IOMcn (
	IOMcn, runIOMcn, (>>>), arr, app,
	getLine, getInt, putLine, isEven) where

import Prelude hiding (getLine)
import qualified Prelude
import Control.Applicative
import Data.Time

newtype IOMcn a b = IOMcn { getIOMcn :: a -> IO b }

runIOMcn :: IOMcn () () -> IO ()
runIOMcn m = getIOMcn m ()

(>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c
m1 >>> m2 = IOMcn $ \x -> getIOMcn m1 x >>= getIOMcn m2

arr :: (a -> b) -> IOMcn a b
arr f = IOMcn $ return . f

app :: IOMcn (IOMcn a b, a) b
app = IOMcn $ uncurry getIOMcn

getLine :: IOMcn () String
getLine = IOMcn $ const Prelude.getLine

getInt :: IOMcn () Int
getInt = IOMcn . const $ fmap read Prelude.getLine

putLine :: IOMcn String ()
putLine = IOMcn putStrLn

isEven :: IOMcn () Bool
isEven = IOMcn . const $ even . floor . utctDayTime <$> getCurrentTime
