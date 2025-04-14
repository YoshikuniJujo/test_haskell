{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.IO where

import Prelude ((.), Show, IO, String, Char)
import Prelude qualified as P
import Control.Monad.Yafee.Eff qualified as Eff
import Control.OpenUnion qualified as Union
import System.IO (Handle)
import System.IO qualified as SIO

-- * STANDARD INPUT/OUTPUT

putChar :: Union.Base IO effs => Char -> Eff.E effs ()
putChar = Eff.effBase . P.putChar

putStr, putStrLn :: Union.Base IO effs => String -> Eff.E effs ()
putStr = Eff.effBase . P.putStr
putStrLn = Eff.effBase . P.putStrLn

print :: (Show a, Union.Base IO effs) => a -> Eff.E effs ()
print = Eff.effBase . P.print

getChar :: Union.Base IO effs => Eff.E effs Char
getChar = Eff.effBase P.getChar

getLine :: Union.Base IO effs => Eff.E effs String
getLine = Eff.effBase P.getLine

-- * HANDLE

hPutChar :: Union.Base IO effs => Handle -> Char -> Eff.E effs ()
hPutChar h = Eff.effBase . SIO.hPutChar h

hPutStr, hPutStrLn :: Union.Base IO effs => Handle -> String -> Eff.E effs ()
hPutStr h = Eff.effBase . SIO.hPutStr h
hPutStrLn h = Eff.effBase . SIO.hPutStrLn h

hPrint :: (Show a, Union.Base IO effs) => Handle -> a -> Eff.E effs ()
hPrint h = Eff.effBase . SIO.hPrint h

hGetChar :: Union.Base IO effs => Handle -> Eff.E effs Char
hGetChar = Eff.effBase . SIO.hGetChar

hGetLine :: Union.Base IO effs => Handle -> Eff.E effs String
hGetLine = Eff.effBase . SIO.hGetLine
