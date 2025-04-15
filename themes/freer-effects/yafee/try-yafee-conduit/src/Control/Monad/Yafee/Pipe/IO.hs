{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.Pipe.IO where

import Prelude hiding (
	putChar, putStr, putStrLn, print,
	getChar )
import Prelude qualified as P

import Foreign.Ptr
import Foreign.Storable
import Control.Monad.Fix
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.Except qualified as Except
import Control.OpenUnion qualified as Union
import System.IO hiding (
	hPutChar, hPutStr, hPutStrLn
	)
import System.IO qualified as IO

-- * TEXT INPUT/OUTPUT

-- ** STANDARD INPUT/OUTPUT

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

-- ** HANDLE

hPutChar :: Union.Base IO effs => Handle -> Eff.E (Pipe.P Char o ': effs) ()
hPutChar h = fix \go ->
	Pipe.await >>= maybe (pure ()) ((>> go) . Eff.effBase . IO.hPutChar h)

hPutStr, hPutStrLn ::
	Union.Base IO effs => Handle -> Eff.E (Pipe.P String o ': effs) ()
hPutStr h = fix \go ->
	Pipe.await >>= maybe (pure ()) ((>> go) . Eff.effBase . IO.hPutStr h)
hPutStrLn h = fix \go ->
	Pipe.await >>= maybe (pure ()) ((>> go) . Eff.effBase . IO.hPutStrLn h)

hPrint ::
	(Show a, Union.Base IO effs) => Handle -> Eff.E (Pipe.P a o ': effs) ()
hPrint h = fix \go ->
	Pipe.await >>= maybe (pure ()) ((>> go) . Eff.effBase . IO.hPrint h)

hGetChar :: Union.Base IO effs => Handle -> Eff.E (Pipe.P i Char ': effs) ()
hGetChar h = fix \go -> Eff.effBase (IO.hGetChar h) >>= Pipe.yield >> go

hGetLine :: Union.Base IO effs => Handle -> Eff.E (Pipe.P i String ': effs) ()
hGetLine h = fix \go -> Eff.effBase (IO.hGetLine h) >>= Pipe.yield >> go

-- * STORABLE

hPutStorable :: (Storable a, Union.Base IO effs) =>
	Handle -> Ptr a -> Eff.E (Pipe.P a o ': effs) ()
hPutStorable h p = fix \go -> Pipe.await >>= maybe (pure ()) \x ->
	Eff.effBase (poke p x) >> Eff.effBase (IO.hPutBuf h p $ sizeOf x) >> go

hGetStorable :: forall i a effs . (
	Storable a,
	Union.Member (Except.E String) effs,
	Union.Base IO effs ) =>
	Handle -> Ptr a -> Eff.E (Pipe.P i a ': effs) ()
hGetStorable h p = fix \go -> do
	rsz <- Eff.effBase $ IO.hGetBuf h p sz
	case rsz of
		0 -> pure ()
		_	| rsz < sz ->
				Except.throw "hGetStorable: Not enough input"
			| rsz == sz -> go
			| otherwise -> Except.throw "never occur"
	where sz = sizeOf (undefined :: a)
