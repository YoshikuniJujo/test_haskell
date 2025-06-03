{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.Lazy (

	-- * HANDLE

	hGet, hGet', hPutStr, hPutStr',

	-- * LENGTH

	lengthRun, length, length',
	Length, lengthToByteString, lengthFromByteString

	) where

import Prelude hiding (length)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as HFunctor
import Data.Bits
import Data.Bool
import Data.Int
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import System.IO (Handle, hIsEOF)

hGet :: (U.Member Pipe.P es, U.Base IO.I es) =>
	Int -> Handle -> Eff.E es i LBS.ByteString ()
hGet bfsz h = fix \go ->  Eff.effBase (not <$> hIsEOF h) >>= bool (pure ())
	(Eff.effBase (BS.hGetSome h bfsz) >>= Pipe.yield . LBS.fromStrict >> go)

hGet' :: (U.Member Pipe.P es, U.Base IO.I es) =>
	Int -> Handle -> Eff.E es i (Maybe LBS.ByteString) ()
hGet' bfsz h = fix \go -> Eff.effBase (not <$> hIsEOF h) >>= bool
	(Pipe.yield Nothing)
	(Eff.effBase (BS.hGetSome h bfsz)
		>>= Pipe.yield . Just . LBS.fromStrict >> go)

hPutStr :: (U.Member Pipe.P es, U.Base IO.I es) =>
	Handle -> Eff.E es LBS.ByteString o r
hPutStr h = fix \go -> (>> go) $ Eff.effBase . LBS.hPut h =<< Pipe.await

hPutStr' :: (U.Member Pipe.P es, U.Base IO.I es) =>
	Handle -> Eff.E es LBS.ByteString o ()
hPutStr' h = fix \go -> Pipe.isMore  >>= bool (pure ())
	((>> go) $ Eff.effBase . LBS.hPut h =<< Pipe.await)

lengthRun :: HFunctor.Loose (U.U es) =>
	Eff.E (State.Named nm Length ': es) i o r -> Eff.E es i o (r, Length)
lengthRun = (`State.runN` (0 :: Length))

length :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Length) es ) =>
	Eff.E es LBS.ByteString LBS.ByteString r
length nm = forever $ Pipe.await >>= \bs ->
	State.modifyN nm (+ Length (LBS.length bs)) >> Pipe.yield bs

length' :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Length) es ) =>
	Eff.E es LBS.ByteString LBS.ByteString ()
length' nm = fix \go -> Pipe.awaitMaybe >>= \case
	Nothing -> pure ()
	Just bs -> (>> go)
		$ State.modifyN nm (+ Length (LBS.length bs)) >> Pipe.yield bs

newtype Length = Length { unLength :: Int64 }
	deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

lengthToByteString :: Int -> Length -> LBS.ByteString
lengthToByteString n0 = LBS.pack . go n0 . unLength
	where
	go n _ | n < 1 = []
	go n ln = fromIntegral ln : go (n - 1) (ln `shiftR` 8)

lengthFromByteString :: Int -> LBS.ByteString -> Maybe Length
lengthFromByteString n0 = (Length <$>) . go n0 . LBS.unpack
	where
	go 0 [] = Just 0
	go n (w : ws)
		| n > 0 = (fromIntegral w .|.) . (`shiftL` 8) <$> go (n - 1) ws
	go _ _ = Nothing
