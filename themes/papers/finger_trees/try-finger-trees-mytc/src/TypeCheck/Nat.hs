{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeCheck.Nat where

import Prelude hiding ((<>))
import Control.Arrow ((***), first, second)
import Data.Maybe
import Data.List

import GhcPlugins
import TcPluginM
import TcRnTypes

import TyCoRep
import TcTypeNats
import TcEvidence	

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const . Just $ TcPlugin {
	tcPluginInit = return (),
	tcPluginSolve = const solveNat,
	tcPluginStop = const $ return () } }

solveNat :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveNat _ _ [] = return $ TcPluginOk [] []
solveNat gs _ ws = do
	tcPluginTrace "!TypeCheck.Nat:" ""
	tcPluginTrace "Given: " $ ppr gs
	tcPluginTrace "!Wanted: " $ ppr ws
	tcPluginTrace "!!LOOKTYPE!!" `mapM_` (uncurry showEqualTypes <$> catMaybes (lookTypesFromCt <$> ws))
	tcPluginTrace "!expression" `mapM_`
		(ppr <$> catMaybes ((tupleSequence . (expression P *** expression P) =<<) . getTypes <$> ws))
	(oks, news) <- unzip . catMaybes <$> makeResult `mapM` ws
	oks' <- catMaybes <$> makeResult2 `mapM` ws
	return $ TcPluginOk (oks ++ oks') news

compareExpressions :: Ct -> Maybe Bool
compareExpressions ct = uncurry (==) <$> (tupleSequence . (expression P *** expression P) =<< getTypes ct)

makeResult2 :: Ct -> TcPluginM (Maybe (EvTerm, Ct))
makeResult2 ct = case compareExpressions ct of
	Just True -> case mev of
		Just ev -> return $ Just (ev, ct)
		Nothing -> error "never occur"
	_ -> return Nothing
	where
	mev = do
		(t1, t2) <- getTypes ct
		return $ makeEvTerm t1 t2

makeResult :: Ct -> TcPluginM (Maybe ((EvTerm, Ct), Ct))
makeResult ct = case evctnt of
	Just (evct, (nt, t2)) -> do
		nct <- makeCt ct nt t2
		return $ Just (evct, nct)
	Nothing -> return Nothing
	where
	evctnt = do
		(t1, t2) <- getTypes ct
		nt <- mkType t1
		return ((makeEvTerm t1 t2, ct), (nt, t2))

showEqualTypes :: SDoc -> SDoc -> SDoc
showEqualTypes a b = a <+> "~" <+> b

look :: Ct -> Maybe SDoc
look (CNonCanonical (CtWanted (TyConApp tc as) dst _nsh _loc)) = Just
	$ "(CNonCanonical (CtWanted " <+> ppr tc <+> ppr as <+> ppr dst <+> "<ctev_nosh>" <+> "<ctev_loc>" <> ")"
look _ = Nothing

lookTypesFromCt :: Ct -> Maybe (SDoc, SDoc)
lookTypesFromCt ct = tupleSequence . (lookType *** lookType) =<< getTypes ct

tupleSequence :: Monad m => (m a, m b) -> m (a, b)
tupleSequence (mx, my) = (,) <$> mx <*> my

getTypes :: Ct -> Maybe (Type, Type)
getTypes ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> Just (t1, t2); _ -> Nothing

lookType :: Type -> Maybe SDoc
lookType (TyConApp tc [a1, TyConApp p [m, o]]) =
	Just $ ppr tc <+> ppr a1 <+> ppr p <+> ppr m <+> ppr o
lookType (TyConApp tc [a, b])
	| tc == typeNatAddTyCon = Just
		$ "typeNatAddTyCon" <+> ppr tc <+> lookDetail a <+> lookDetail b
	| tc == typeNatSubTyCon = Just
		$ "typeNatSubTyCon" <+> ppr tc <+> lookDetail a <+> lookDetail b
	| otherwise = Just
		$ "others" <+> ppr tc <+> lookDetail a <+> lookDetail b
lookType (TyConApp tc []) = Just $ ppr tc <+> "no arg"
lookType _ = Nothing

lookDetail :: Type -> SDoc
lookDetail (TyVarTy v) = "TyVarTy" <+> ppr v
lookDetail (LitTy (NumTyLit n)) = "LitTy (NumTyLit" <+> ppr n <+> ")"
lookDetail t = ppr t

data NP = N | P deriving (Show, Eq, Ord)

neg :: NP -> NP
neg = \case N -> P; P -> N

instance Outputable NP where ppr N = "N"; ppr P = "P"

expression :: NP -> Type -> Maybe ([(NP, Var)], Integer)
expression np t = (sort *** calcInteger) <$> separateTerms <$> parseExpression np t

calcInteger :: [(NP, Integer)] -> Integer
calcInteger [] = 0
calcInteger ((N, n) : ns) = - n + calcInteger ns
calcInteger ((P, n) : ns) = n + calcInteger ns

separateTerms :: [(NP, Either Var Integer)] -> ([(NP, Var)], [(NP, Integer)])
separateTerms [] = ([], [])
separateTerms ((np, Left v) : ts) = ((np, v) :) `first` separateTerms ts
separateTerms ((np, Right n) : ts) = ((np, n) :) `second` separateTerms ts

parseExpression :: NP -> Type -> Maybe [(NP, Either Var Integer)]
parseExpression np (TyVarTy v) = Just [(np, Left v)]
parseExpression np (LitTy (NumTyLit n)) = Just [(np, Right n)]
parseExpression np (TyConApp tc [a, b])
	| tc == typeNatAddTyCon = do
		ea <- parseExpression np a
		eb <- parseExpression np b
		return $ ea ++ eb
	| tc == typeNatSubTyCon = do
		ea <- parseExpression np a
		eb <- parseExpression (neg np) b
		return $ ea ++ eb
parseExpression _ _ = Nothing

mkTypeFromCt :: Ct -> Maybe Type
mkTypeFromCt ct = mkType . fst =<< getTypes ct

mkType :: Type -> Maybe Type
mkType (TyConApp tc [a1, TyConApp _p [m, _o]])
	| tc ==typeNatLeqTyCon = Just $ TyConApp tc [a1, m]
mkType _ = Nothing

makeCt :: Ct -> Type -> Type -> TcPluginM Ct
makeCt ct t1 t2 = do
	hole <- newCoercionHole pt
	return . mkNonCanonical $ CtWanted pt (HoleDest hole) WDeriv (ctLoc ct)
	where pt = mkPrimEqPred t1 t2

makeEvTerm :: Type -> Type -> EvTerm
makeEvTerm t1 t2 = EvExpr . Coercion
	$ mkUnivCo (PluginProv "TypeCheck.Nat") Nominal t1 t2
