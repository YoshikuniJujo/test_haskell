{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryNonDet where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.NonDet qualified as NonDet
import Control.OpenUnion qualified as Union
import Data.List qualified as L

import Debug.Trace

type KnightPos = (Int, Int)

moveKnight :: Union.Member NonDet.N effs => KnightPos -> Eff.E effs KnightPos
moveKnight (c, r) = do
	(c', r') <- asum $ pure <$> [
		(c + 2, r - 1), (c + 2, r + 1), (c - 2, r - 1), (c - 2, r + 1),
		(c + 1, r - 2), (c + 1, r + 2), (c - 1, r - 2), (c - 1, r + 2) ]
	guard $ c' `elem` [1 .. 8] && r' `elem` [1 .. 8]
	pure (c', r')

in3 :: Union.Member NonDet.N effs => KnightPos -> Eff.E effs KnightPos
in3 st = do
	fs <- moveKnight st
	sc <- moveKnight fs
	moveKnight sc

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 st ed = ed `elem` (Eff.run . NonDet.run $ in3 st :: [KnightPos])

data LocalMatch = LocalMatch Int Bool deriving Show

data GlobalMatch = GlobalMatch Int deriving Show

matchSelf (LocalMatch n _) = LocalMatch n True

compareWithMatch n1 = do
	LocalMatch n0 slf <- State.get
	guard $ n0 < 1 || (n1 > n0 || n1 == n0 && slf)
	State.put $ LocalMatch 0 False

abcOrBcde :: (
	Union.Member (State.S String) effs,
	Union.Member (State.S Int) effs,
	Union.Member (State.S [(Int, Int)]) effs,
	Union.Member (State.S LocalMatch) effs,
	Union.Member (State.S GlobalMatch) effs,
	Union.Member NonDet.N effs
	) =>
	Eff.E effs ()
abcOrBcde = State.get >>= \case
	"" -> pure ()
	s -> do	p :: Int <- State.get
		case ("abc" `L.isPrefixOf` s, "bcde" `L.isPrefixOf` s) of
			(True, _) -> do
				compareWithMatch 3
				State.put $ LocalMatch 3 False
				do			State.modify matchSelf
							State.modify ((p, 3 :: Int) :)
							State.put $ drop 3 s
							State.put $ p + 3
					<|>	do	State.put $ drop 1 s
							State.put $ p + 1
			(_, True) -> do
				compareWithMatch 4
				State.put $ LocalMatch 4 False
				do			State.modify matchSelf
							State.modify ((p, 4 :: Int) :)
							State.put $ drop 4 s
							State.put $ p + 4
			    		<|>	do	State.put $ drop 1 s
							State.put $ p + 1
			_ -> do compareWithMatch 0
				State.put $ LocalMatch 0 False
				State.put $ drop 1 s
				State.put $ p + 1
		abcOrBcde

longestMatch :: [String] -> String -> Maybe (Int, String)
longestMatch ps str = case filter (`L.isPrefixOf` str) ps of
	[] -> Nothing
	ms -> maxLenString ms

maxLenString :: [String] -> Maybe (Int, String)
maxLenString strs = case (length &&& id) <$> strs of
	[] -> Nothing
	ms -> Just $ maximum ms

newtype GlobalLength = GlobalLength Int deriving (Show, Eq, Ord)
newtype LocalLength = LocalLength Int deriving Show

checkAbcOrBcde str = Eff.run
	. (`State.run` GlobalMatch 0)
	. NonDet.run
	. (`State.run` LocalMatch 0 False)
	. (`State.run` str) . (`State.run` (0 :: Int)) $ (`State.run` ([] :: [(Int, Int)])) abcOrBcde

matchWith ps = State.get >>= \case
	"" -> pure ()
	s -> do	p <- State.get
		case longestMatch ps s of
			Nothing -> do
				State.put $ p + 1
				State.put $ tail s
				matchWith ps
			Just (ln, m) -> do
				do		State.modify ((p, ln, m) :)
						State.put $ p + ln
						State.put $ drop ln s
						State.modify (GlobalLength ln `max`)
						State.put $ LocalLength ln
					<|> do	State.put $ p + 1
						State.put $ tail s
						matchWithAhead ps
		GlobalLength g <- State.get
		LocalLength l <- State.get
		trace (show (g, l)) $ pure ()
		guard $ l >= g
		matchWith ps
	
matchWithAhead ps = State.get >>= \case
	"" -> pure ()
	s -> do p <- State.get
		case longestMatch ps s of
			Nothing -> guard False
			Just (ln, m) -> do
				State.modify ((p, ln, m) :)
				State.put $ p + ln
				State.put $ drop ln s
				State.modify (GlobalLength ln `max`)
				State.put $ LocalLength ln

checkMatch ps s = Eff.run
	. (`State.run` GlobalLength 0)
	. NonDet.run
	. (`State.run` LocalLength 0)
	. (`State.run` (0 :: Int))
	. (`State.run` ([] :: [(Int, Int, String)]))
	. (`State.run` s)
	$ matchWith ps
