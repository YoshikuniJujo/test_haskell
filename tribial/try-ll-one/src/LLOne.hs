{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LLOne (go, Rules, Rule, Symbol(..), TokenIs, BuildTree) where

import Control.Arrow
import Control.Monad.MaybeState

go :: Rules nt tkn tr -> (tkn -> tr) -> nt -> (String -> [tkn]) -> String -> Maybe tr
go rls ttt nt0 lxr src = case parse rls [Nonterminal nt0] $ lxr src of
	(_, False) -> Nothing
	(rts, True) -> case (mkTree ttt) `runMaybeState` rts of
		Just (r, []) -> Just r; _ -> Nothing

data Symbol nt t = Nonterminal nt | Terminal (TokenIs t)
type TokenIs t = t -> Bool
data RuleOrToken rn tkn = Rule rn [HoleOrToken tkn] | Token tkn

ruleOrTokenToToken :: RuleOrToken rn tkn -> tkn
ruleOrTokenToToken (Token tkn) = tkn
ruleOrTokenToToken _ = error "bad"

type BuildTree tr = [tr] -> tr

type Rules nt tkn tr = [(nt -> tkn -> Bool, Rule nt tkn tr)]
type Rule nt tkn tr = ([Symbol nt tkn] , BuildTree tr)

data HoleOrToken tkn = Hole | HTToken (TokenIs tkn)

ruleToHoleOrTokens :: [Symbol nt tkn] -> [HoleOrToken tkn]
ruleToHoleOrTokens = (r2ht <$>)
	where r2ht = \case Nonterminal _ -> Hole; Terminal tkn -> HTToken tkn

getRule :: Rules nt tkn tr -> nt -> tkn -> Maybe (Rule nt tkn tr)
getRule [] _ _ = Nothing
getRule ((p, r) : prs) nt tkn
	| p nt tkn = Just r
	| otherwise = getRule prs nt tkn

parse :: Rules nt tkn tr -> [Symbol nt tkn] -> [tkn] -> ([RuleOrToken (BuildTree tr) tkn], Bool)
parse rls (Nonterminal nt : ss) ta@(t : _) = case getRule rls nt t of
	Just (rl, rlnm) -> (Rule rlnm (ruleToHoleOrTokens rl) :) `first` parse rls (rl ++ ss) ta
	_ -> ([], False)
parse rls (Terminal t0 : st) (t : ts) | t0 t = (Token t :) `first` parse rls st ts
parse _ [] [] = ([], True)
parse _ _ _ = ([], False)

mkTree :: (tkn -> tr) -> MaybeState [RuleOrToken (BuildTree tr) tkn] tr
mkTree ttt = getHead >>= \case
	Rule mkt hts -> mkt <$> holeOrTokensToTrees ttt hts (mkTree ttt)
	_ -> fail ""

holeOrTokensToTrees ::
	(tkn -> tr) ->
	[HoleOrToken tkn] ->
	MaybeState [RuleOrToken (BuildTree tr) tkn] tr ->
	MaybeState [RuleOrToken (BuildTree tr) tkn] [tr]
holeOrTokensToTrees _ [] _ = fail ""
holeOrTokensToTrees _ [Hole] mkt = (: []) <$> mkt
holeOrTokensToTrees ttt [HTToken t] _ =
	((: []) . ttt . ruleOrTokenToToken <$>) . checkHead $ \case Token tkn -> t tkn; _ -> False
holeOrTokensToTrees ttt (Hole : hts) mkt = (:) <$> mkt <*> holeOrTokensToTrees ttt hts mkt
holeOrTokensToTrees ttt (HTToken t : hts) mkt = (:)
	<$> (ttt . ruleOrTokenToToken <$> checkHead \case Token tkn -> t tkn; _ -> False)
	<*> holeOrTokensToTrees ttt hts mkt
