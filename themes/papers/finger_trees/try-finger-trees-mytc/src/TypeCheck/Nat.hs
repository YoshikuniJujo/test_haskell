{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeCheck.Nat where

import Prelude hiding ((<>))
import Control.Arrow ((***))
import Data.Maybe

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
	tcPluginTrace "Wanted: " $ ppr ws
--	tcPluginTrace "!!LOOK!!" `mapM_` catMaybes (look <$> ws)
--	(tcPluginTrace "Types" . ppr) `mapM_` catMaybes (getTypes <$> ws)
	tcPluginTrace "!!LOOKTYPE!!" `mapM_` (uncurry showEqualTypes <$> catMaybes (lookTypesFromCt <$> ws))
--	(tcPluginTrace "mkType" . ppr) `mapM_` catMaybes (mkTypeFromCt <$> ws)
--	((tcPluginTrace "result" . ppr) `mapM_`) . catMaybes =<< makeResult `mapM` ws
	(oks, news) <- unzip . catMaybes <$> makeResult `mapM` ws
	return $ TcPluginOk oks news
--	return $ TcPluginOk [] []

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
lookType (TyConApp tc [a1, TyConApp p [m, o]]) = Just $ ppr tc <+> ppr a1 <+> ppr p <+> ppr m <+> ppr o
lookType (TyConApp tc as) = Just $ ppr tc <+> ppr as
lookType _ = Nothing

mkTypeFromCt :: Ct -> Maybe Type
mkTypeFromCt ct = mkType . fst =<< getTypes ct

mkType :: Type -> Maybe Type
mkType (TyConApp tc [a1, TyConApp _p [m, _o]]) = Just $ TyConApp tc [a1, m]
mkType _ = Nothing

makeCt :: Ct -> Type -> Type -> TcPluginM Ct
makeCt ct t1 t2 = do
	hole <- newCoercionHole pt
	return . mkNonCanonical $ CtWanted pt (HoleDest hole) WDeriv (ctLoc ct)
	where pt = mkPrimEqPred t1 t2

makeEvTerm :: Type -> Type -> EvTerm
makeEvTerm t1 t2 = EvExpr . Coercion
	$ mkUnivCo (PluginProv "TypeCheck.Nat") Nominal t1 t2
