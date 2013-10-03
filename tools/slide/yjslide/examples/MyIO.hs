module MyIO where

import Prelude hiding (getChar, putChar, getLine, putStrLn)
import qualified Prelude as P

class IOClass io where
	pipe :: io a b -> io b c -> io a c
	trans :: (a -> b) -> io b c -> io a c

data MyIO i o = MyIO { runMyIO :: i -> IO o }

run :: MyIO () o -> IO o
run = ($ ()) . runMyIO 

apply :: IOClass io => io b c -> io a b -> io a c
apply = flip pipe

instance IOClass MyIO where
	pipe (MyIO i) (MyIO o) = MyIO $ \x -> i x >>= o
	trans f (MyIO io) = MyIO $ io . f

getChar :: MyIO () Char
getChar = MyIO $ const P.getChar

putChar :: MyIO Char ()
putChar = MyIO P.putChar

getLine :: MyIO () String
getLine = MyIO $ const P.getLine

putLine :: MyIO String ()
putLine = MyIO P.putStrLn
