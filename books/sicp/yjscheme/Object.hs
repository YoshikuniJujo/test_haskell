{-# LANGUAGE PackageImports #-}

module Object (
	SchemeM,
	Object(OInt, ODouble, ORational, OString, OVar, ONil, OBool, OUndef,
		OError, OSubr, OSyntax, OClosure),
	showObjM,
	showObj,
	imcons,
	cons,
	car, cdr,
	cons2list,

	Environment, mkInitEnv,
	EnvT, runEnvT, runSchemeM,
	define, getValue, getEID, newEnv, popEnv,
	throwError, catchError,
	EID,

	lastCons, foldlCons, zipWithCons, mapCons,
) where

import Env

import Data.Ratio
import Data.Maybe
import Data.Time

import "monads-tf" Control.Monad.Reader
import "monads-tf" Control.Monad.State
import Control.Applicative

type SchemeM = EnvT Object (StateT ConsEnv (ReaderT UTCTime IO))

type ConsEnv = (CID, [(CID, Object)])
type CID = Int

nCons :: Monad m => Object -> Object -> ConsEnv -> m ((CID, CID), ConsEnv)
nCons a d (mcid, cenv) = return ((mcid + 1, mcid), (mcid + 2, (mcid + 1, a) : (mcid, d) : cenv))
getC :: CID -> ConsEnv -> Object
getC cid (_, cenv) = fromJust $ lookup cid cenv

runSchemeM :: UTCTime -> Environment Object -> SchemeM a -> IO a
runSchemeM it env = (`runReaderT` it) . flip evalStateT (0, []) . runEnvT env

data Object
	= OInt Integer
	| ODouble Double
	| ORational Rational
	| OString String
	| OVar String
	| OCons Object Object
	| OMCons CID CID
	| ONil
	| OBool Bool
	| OUndef
	| OError
	| OSubr String (Object -> SchemeM Object)
	| OSyntax String (Object -> SchemeM Object)
	| OClosure (Maybe String) EID Object Object

imcons :: Object -> Object -> Object
imcons a d = OCons a d

cons :: Object -> Object -> SchemeM Object
cons a d = do
	(aid, did) <- lift $ lift $ StateT $ nCons a d
	return $ OMCons aid did

car, cdr :: String -> Object -> SchemeM Object
car _ (OCons a _) = return a
car _ (OMCons a _) = lift $ gets $ getC a
car err _ = throwError err
cdr _ (OCons _ d) = return d
cdr _ (OMCons _ d) = lift $ gets $ getC d
cdr err _ = throwError err

cons2list :: Object -> SchemeM [Object]
cons2list ONil = return []
cons2list o = (:) <$> car err o <*> (cons2list =<< cdr err o)
	where
	err = "*** ERROR: not list: " ++ showObj o

showObjM :: Object -> SchemeM String
showObjM o@(OMCons _ _) = showConsM False o
{- do
	a <- showObjM =<< car "" o
	d <- showObjM =<< cdr "" o
	return $ "(" ++ a ++ " . " ++  d ++ ")" -}
showObjM o = return $ showObj o

showObj :: Object -> String
showObj (OInt i) = show i
showObj (ODouble d) = show d
showObj (ORational r) = show (numerator r) ++ "/" ++ show (denominator r)
showObj (OString s) = show s
showObj (OVar v) = v
showObj (OCons (OVar "quote") (OCons a ONil)) = "'" ++ showObj a
showObj c@(OCons _ _) = showCons False c
showObj (OMCons c1 c2) = "(#" ++ show c1 ++ " . #" ++ show c2 ++ ")"
showObj ONil = "()"
showObj (OBool True) = "#t"
showObj (OBool False) = "#f"
showObj OUndef = "#<undef>"
showObj (OSubr n _) = "#<subr " ++ n ++ ">"
showObj (OSyntax n _) = "#<syntax " ++ n ++ ">"
showObj (OClosure n _ _ _) = "#<closure " ++ fromMaybe "#f" n ++ ">"
showObj OError = "#<error>"

showCons :: Bool -> Object -> String
showCons l (OCons a d) = (if l then id else ("(" ++ ) . (++ ")")) $
	case d of
		OCons _ _ -> showObj a ++ " " ++ showCons True d
		ONil -> showObj a
		_ -> showObj a ++ " . " ++ showObj d
showCons l ONil = if l then "" else "()"
showCons _ _ = error "not cons"

showConsM :: Bool -> Object -> SchemeM String
showConsM l ONil = return $ if l then "" else "()"
showConsM l o = do
	a <- car "not cons" o
	d <- cdr "not cons" o
	(if l then id else ("(" ++) . (++ ")")) <$>
		case d of
			OCons _ _ -> do
				sa <- showObjM a
				sd <- showConsM True d
				return $ sa ++ " " ++ sd
			OMCons _ _ -> do
				sa <- showObjM a
				sd <- showConsM True d
				return $ sa ++ " " ++ sd
			ONil -> showObjM a
			_ -> do sa <- showObjM a
				sd <- showObjM d
				return $ sa ++ " . " ++ sd

lastCons :: Object -> SchemeM Object
lastCons o = do
	a <- car err o
	d <- cdr err o
	case d of
		ONil -> return a
		_ -> lastCons d
	where
	err = "*** ERROR: lastCons: not list: " ++ showObj o

foldlCons :: (Object -> Object -> SchemeM Object) -> Object -> Object ->
	SchemeM Object
foldlCons _ o0 ONil = return o0
foldlCons f o0 os = do
	a <- car err os
	d <- cdr err os
	r <- f o0 a
	foldlCons f r d
	where
	err = "foldlCons: bad"

zipWithCons :: (Object -> Object -> SchemeM Object) -> Object -> Object ->
	SchemeM Object
zipWithCons _ ONil _ = return ONil
zipWithCons _ _ ONil = return ONil
zipWithCons f o o' = do
	a <- car err o
	d <- cdr err o
	a' <- car err o'
	d' <- cdr err o'
	ra <- f a a'
	rd <- zipWithCons f d d'
	cons ra rd
	where
	err = "zipWithCons: bad"

mapCons :: (Object -> SchemeM Object) -> Object -> SchemeM Object
mapCons _ ONil = return ONil
mapCons f o = do
	a <- f =<< car err o
	d <- mapCons f =<< cdr err o
	cons a d
	where
	err = "*** ERROR: proper list required for function application or macro use: "
		++ showObj o
