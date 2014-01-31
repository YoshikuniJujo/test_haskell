module IOMcn (
	IOMcn, runIOMcn, (>>>), arr, app,
	putLine, getLine, getInt, isEven) where

import Prelude hiding (getLine)
import qualified Prelude
import Control.Arrow hiding (arr, (>>>))
import qualified Control.Arrow
import Control.Applicative
import Data.Time

type IOMcn = Kleisli IO

runIOMcn :: IOMcn () a -> IO a
runIOMcn = (`runKleisli` ())

(>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c
(>>>) = (Control.Arrow.>>>)

arr :: (a -> b) -> IOMcn a b
arr = Control.Arrow.arr

putLine :: IOMcn String ()
putLine = Kleisli putStrLn

getLine :: IOMcn () String
getLine = Kleisli $ const Prelude.getLine

getInt :: IOMcn () Int
getInt = Kleisli $ const $ Prelude.getLine >>= readIO

isEven :: IOMcn () Bool
isEven = Kleisli $ const $
	(even :: Int -> Bool) . floor . utctDayTime <$> getCurrentTime
