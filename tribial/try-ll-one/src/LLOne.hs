{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LLOne (go, Rules, Rule, Symbol(..), TokenIs, BuildTree) where

import Control.Arrow (first)
import Control.Monad.MaybeState (MaybeState, runMaybeState, getHead, checkHead)

go :: Rules nt tk tr ->
	(tk -> tr) -> nt -> (String -> [tk]) -> String -> Maybe tr
go rls ttt nt0 lxr src = case parse rls [Nonterminal nt0] $ lxr src of
	(_, False) -> Nothing
	(rts, True) -> case tree ttt `runMaybeState` rts of
		Just (r, []) -> Just r; _ -> Nothing

parse :: Rules nt tk tr -> [Symbol nt tk] -> [tk] -> ([RuleToken tk tr], Bool)
parse rls (Nonterminal nt : ss) ta@(tkn : _) = case getRule rls nt tkn of
	Just (nss, mkt) ->
		(Rule (toHoles nss) mkt :) `first` parse rls (nss ++ ss) ta
	_ -> ([], False)
parse rls (Terminal p : st) (t : ts) | p t = (Token t :) `first` parse rls st ts
parse _ [] [] = ([], True); parse _ _ _ = ([], False)

getRule :: Rules nt tk tr -> nt -> tk -> Maybe (Rule nt tk tr)
getRule [] _ _ = Nothing
getRule ((p, r) : prs) nt tk | p nt tk = Just r | otherwise = getRule prs nt tk

toHoles :: [Symbol nt tk] -> [HoleToken tk]
toHoles = map \case Nonterminal _ -> Hole; Terminal tk -> HTT tk

type Rules nt tk tr = [(nt -> tk -> Bool, Rule nt tk tr)]
type Rule nt tk tr = ([Symbol nt tk] , BuildTree tr)
type BuildTree tr = [tr] -> tr
data Symbol nt t = Nonterminal nt | Terminal (TokenIs t)
type TokenIs t = t -> Bool

tree :: (tk -> tr) -> MaybeState [RuleToken tk tr] tr
tree ttt = getHead >>= \case
	Rule hts mkt -> mkt <$> htsToTree ttt (tree ttt) hts
	_ -> fail ""

htsToTree :: (tk -> tr) -> MaybeState [RuleToken tk tr] tr ->
	[HoleToken tk] -> MaybeState [RuleToken tk tr] [tr]
htsToTree _ _ [] = pure []
htsToTree ttt mkt (Hole : hts) = (:) <$> mkt <*> htsToTree ttt mkt hts
htsToTree ttt mkt (HTT p : hts) = (:)
	<$> (ttt . rtToToken <$> checkHead \case Token tk -> p tk; _ -> False)
	<*> htsToTree ttt mkt hts

rtToToken :: RuleToken tk tr -> tk
rtToToken (Token tk) = tk; rtToToken _ = error "bad"

data RuleToken tk tr = Rule [HoleToken tk] (BuildTree tr) | Token tk
data HoleToken tk = Hole | HTT (TokenIs tk)
