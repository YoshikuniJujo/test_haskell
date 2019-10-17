{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypecheckPlusone.Plugin where

import Data.Maybe

import GhcPlugins
import TcPluginM
import TcRnTypes
import TyCoRep
import TcEvidence
import TcTypeNats

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const . Just $ TcPlugin {
	tcPluginInit = return (),
	tcPluginSolve = const $ solvePlusOnePlugin,
	tcPluginStop = const $ return () } }

solvePlusOnePlugin :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solvePlusOnePlugin gs _ ws = do
	(oks, news) <- unzip . catMaybes <$> solvePlusOne gs `mapM` ws
	return $ TcPluginOk oks news

solvePlusOne :: [Ct] -> Ct -> TcPluginM (Maybe ((EvTerm, Ct), Ct))
solvePlusOne gs w
	| Just (t1, t2) <- getTypes w = do
		if any (`isPlusOneOf` t1) gts && any (`isPlusOneOf` t2) gts
		then do	nct <- makeCt w (plusOne t1) (plusOne t2)
			return $ Just ((makeEvTerm t1 t2, w), nct)
		else return Nothing
	| otherwise = return Nothing
	where gts = allTypes gs

getTypes :: Ct -> Maybe (Type, Type)
getTypes ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> Just (t1, t2); _ -> Nothing

allTypes :: [Ct] -> [Type]
allTypes cts =
	concat . catMaybes $ ((\(t1, t2) -> [t1, t2]) <$>) . getTypes <$> cts

plusOne :: Type -> Type
plusOne t = TyConApp typeNatAddTyCon [t, LitTy $ NumTyLit 1]

isPlusOneOf :: Type -> Type -> Bool
TyConApp tc [TyVarTy t1, LitTy (NumTyLit 1)] `isPlusOneOf` TyVarTy t =
	tc == typeNatAddTyCon && t1 == t
_ `isPlusOneOf` _ = False

makeCt :: Ct -> Type -> Type -> TcPluginM Ct
makeCt ct t1 t2 = do
	hole <- newCoercionHole pt
	return . mkNonCanonical $ CtWanted pt (HoleDest hole) WDeriv (ctLoc ct)
	where pt = mkPrimEqPred t1 t2

makeEvTerm :: Type -> Type -> EvTerm
makeEvTerm t1 t2 = EvExpr . Coercion
	$ mkUnivCo (PluginProv "TypecheckPlusone.Plugin") Nominal t1 t2
