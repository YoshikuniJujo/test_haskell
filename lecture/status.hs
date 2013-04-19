module ExplainIO (
	State,
	xset, yset, xadd, yadd, getx, gety,

	xsetIO, ysetIO, xaddIO, yaddIO, getxIO, getyIO
) where

import Control.Applicative
import System.IO

type State s a = s -> (a, s)

xset :: Int -> State (Int, Int) ()
xset n (_, y) = ((), (n, y))

yset :: Int -> State (Int, Int) ()
yset n (x, _) = ((), (x, n))

xadd :: Int -> State (Int, Int) ()
xadd n (x, y) = ((), (x + n, y))

yadd :: Int -> State (Int, Int) ()
yadd n (x, y) = ((), (x, y + n))

getx :: State (Int, Int) Int
getx s@(x, _) = (x, s)

gety :: State (Int, Int) Int
gety s@(_, y) = (y, s)

(>>>) :: State s a -> State s b -> State s b
(f1 >>> f2) s0 = let (x, s1) = f1 s0 in f2 s1

(>>>=) :: State s a -> (a -> State s b) -> State s b
(f1 >>>= f2) s0 = let (x, s1) = f1 s0 in f2 x s1

stateEx = xset 8 >>> getx >>>= yadd >>> gety

xsetIO :: Int -> IO ()
xsetIO = writeFile' "x" . show

ysetIO :: Int -> IO ()
ysetIO = writeFile' "y" . show

getxIO :: IO Int
getxIO = read <$> readFile' "x"

getyIO :: IO Int
getyIO = read <$> readFile' "y"

xaddIO :: Int -> IO ()
xaddIO n = xsetIO . (+ n) =<< getxIO

yaddIO :: Int -> IO ()
yaddIO n = ysetIO . (+ n) =<< getyIO

initialize :: (Int, Int) -> IO ()
initialize (x, y) = xsetIO x >> ysetIO y

ioEx = xsetIO 8 >> getxIO >>= yaddIO >> getyIO

writeFile' fp str = withFile fp WriteMode $ flip hPutStr str
readFile' fp = withFile fp ReadMode hGetLine
