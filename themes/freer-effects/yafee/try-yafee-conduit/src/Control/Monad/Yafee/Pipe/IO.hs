{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.Pipe.IO where

import Prelude hiding (
	putChar, putStr, putStrLn, print,
	getChar )
import Prelude qualified as P
import Control.Monad.Fix
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.OpenUnion qualified as Union

-- * STANDARD INPUT/OUTPUT

putChar :: Union.Base IO effs => Eff.E (Pipe.P Char o ': effs) ()
putChar = fix \go ->
	Pipe.await >>= maybe (pure ()) ((>> go) . Eff.effBase . P.putChar)

putStr, putStrLn :: Union.Base IO effs => Eff.E (Pipe.P String o ': effs) ()
putStr = fix \go ->
	Pipe.await >>= maybe (pure ()) ((>> go) . Eff.effBase . P.putStr)
putStrLn = fix \go ->
	Pipe.await >>= maybe (pure ()) ((>> go) . Eff.effBase . P.putStrLn)

print :: (Show a, Union.Base IO effs) => Eff.E (Pipe.P a o ': effs) ()
print = fix \go ->
	Pipe.await >>= maybe (pure ()) ((>> go) . Eff.effBase . P.print)

getChar :: Union.Base IO effs => Eff.E (Pipe.P i Char ': effs) ()
getChar = fix \go -> Eff.effBase P.getChar >>= Pipe.yield >> go

getLine :: Union.Base IO effs => Eff.E (Pipe.P i String ': effs) ()
getLine = fix \go -> Eff.effBase P.getLine >>= Pipe.yield >> go

-- * HANDLE
