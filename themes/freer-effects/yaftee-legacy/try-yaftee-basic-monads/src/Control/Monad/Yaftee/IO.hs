{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.IO where

import Prelude ((.), Show, IO, String, Char)
import Prelude qualified as P
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.HigherOpenUnion qualified as Union
import System.IO (Handle)
import System.IO qualified as SIO

-- * TYPE

type I = Union.FromFirst IO

-- * STANDARD INPUT/OUTPUT

putChar :: Union.Base I effs => Char -> Eff.E effs ()
putChar = Eff.effBase . P.putChar

putStr, putStrLn :: Union.Base I effs => String -> Eff.E effs ()
putStr = Eff.effBase . P.putStr
putStrLn = Eff.effBase . P.putStrLn

print :: (Show a, Union.Base I effs) => a -> Eff.E effs ()
print = Eff.effBase . P.print

getChar :: Union.Base I effs => Eff.E effs Char
getChar = Eff.effBase P.getChar

getLine :: Union.Base I effs => Eff.E effs String
getLine = Eff.effBase P.getLine

-- * HANDLE

hPutChar :: Union.Base I effs => Handle -> Char -> Eff.E effs ()
hPutChar = (Eff.effBase .) . SIO.hPutChar

hPutStr, hPutStrLn :: Union.Base I effs => Handle -> String -> Eff.E effs ()
hPutStr = (Eff.effBase .) . SIO.hPutStr
hPutStrLn = (Eff.effBase .) . SIO.hPutStrLn

hPrint :: (Show a, Union.Base I effs) => Handle -> a -> Eff.E effs ()
hPrint = (Eff.effBase .) . SIO.hPrint

hGetChar :: Union.Base I effs => Handle -> Eff.E effs Char
hGetChar = Eff.effBase . SIO.hGetChar

hGetLine :: Union.Base I effs => Handle -> Eff.E effs String
hGetLine = Eff.effBase . SIO.hGetLine
