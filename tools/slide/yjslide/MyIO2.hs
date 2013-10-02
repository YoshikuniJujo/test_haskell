module MyIO2 where

import Prelude hiding (getLine)
import qualified Prelude as P

type MyIO i o = i -> IO o

pipe :: MyIO a b -> MyIO b c -> MyIO a c
pipe i o = \x -> i x >>= o

trans :: (a -> b) -> MyIO b c -> MyIO a c
trans = flip (.)

(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>>) = flip (.)

trans' :: MyIO b c -> (a -> b) -> MyIO a c
trans' = flip trans

getLine :: MyIO () String
getLine = const P.getLine

putLine :: MyIO String ()
putLine = putStrLn

run :: MyIO () o -> IO o
run = ($ ())
