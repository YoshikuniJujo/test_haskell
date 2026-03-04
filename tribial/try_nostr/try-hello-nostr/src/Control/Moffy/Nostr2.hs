{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

{-# LANGUAGE ExplicitForAll, RequiredTypeArguments #-}

module Control.Moffy.Nostr2 (sampleId2) where

import Prelude hiding (break, scanl)
import Control.Monad
import Control.Concurrent.STM
import Control.Moffy
import Control.Moffy.Nostr.Event
import Control.Moffy.Nostr.Run
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Nostr.Event qualified as Event
import Nostr.Filter qualified as Filter
import Tools
import TryBech32

sampleId2 :: FilePath -> IO ()
sampleId2 fp = do
	Just flt <- filterP <$> T.readFile fp
	run display "nos.lol" "443" do
		waitFor $ request "req-p" flt
		void $ parList (spawn evPair) `break` end
		waitFor halt
	where
	display e ms = prp `mapM_` ms >> atomically case ms of
		[] -> readTVar e >>= \case Now -> writeTVar e End; _ -> pure ()
		_ -> readTVar e >>= \case Pre -> writeTVar e Now; _ -> pure ()
	prp (ev, mev) = do
		print ev
		T.putStrLn $ Event.content ev
		print $ lookupE ev
		case mev of
			Nothing -> putStrLn "NO ROOT MESSAGE"
			Just ev' -> print ev' >> T.putStrLn (Event.content ev')
	evPair = waitFor (awaitNameEvent "req-p") >>= \ev -> case lookupE ev of
		Nothing -> emit (ev, Nothing)
		Just e -> do
			emit (ev, Nothing)
			waitFor $ request nm (filterId e)
			emit . (ev ,) . Just =<< waitFor (awaitNameEvent nm)
			where nm = "id-" <> e

lookupE :: Event.E -> Maybe T.Text
lookupE = (fst <$>) . lookup "e" . Event.tags

filterP :: T.Text -> Maybe Filter.Filter
filterP a = do
	pub <- toHex <$> dataPart' "npub" (chomp a)
	pure Filter.Filter {
		Filter.ids = Nothing,
		Filter.authors = Nothing, Filter.kinds = Just [1],
		Filter.tags = [('p', [pub])],
		Filter.since = Nothing, Filter.until = Nothing,
		Filter.limit = Just 5 }

filterId :: T.Text -> Filter.Filter
filterId a = Filter.Filter {
	Filter.ids = Just [fromHex a],
	Filter.authors = Nothing, Filter.kinds = Just [1], Filter.tags = [],
	Filter.since = Nothing, Filter.until = Nothing, Filter.limit = Just 1 }
