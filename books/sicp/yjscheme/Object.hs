{-# LANGUAGE PackageImports #-}

module Object (
	SchemeM,
	Object(OInt, ODouble, ORational, OString, OVar, ONil, OBool, OUndef,
		OError, OSubr, OSyntax, OClosure),
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
import Control.Applicative

type SchemeM = EnvT Object (ReaderT UTCTime IO)

runSchemeM :: UTCTime -> Environment Object -> SchemeM a -> IO a
runSchemeM it env = (`runReaderT` it) . runEnvT env

data Object
	= OInt Integer
	| ODouble Double
	| ORational Rational
	| OString String
	| OVar String
	| OCons Object Object
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
cons a d = return $ OCons a d

car, cdr :: String -> Object -> SchemeM Object
car _ (OCons a _) = return a
car err _ = throwError err
cdr _ (OCons _ d) = return d
cdr err _ = throwError err

cons2list :: Object -> SchemeM [Object]
cons2list ONil = return []
cons2list (OCons a d) = (a :) <$> cons2list d
cons2list o = throwError $ "*** ERROR: not list: " ++ showObj o

showObj :: Object -> String
showObj (OInt i) = show i
showObj (ODouble d) = show d
showObj (ORational r) = show (numerator r) ++ "/" ++ show (denominator r)
showObj (OString s) = show s
showObj (OVar v) = v
showObj (OCons (OVar "quote") (OCons a ONil)) = "'" ++ showObj a
showObj c@(OCons _ _) = showCons False c
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

lastCons :: Object -> SchemeM Object
lastCons (OCons a ONil) = return a
lastCons (OCons _ d) = lastCons d
lastCons o = throwError $ "*** ERROR: lastCons: not list: " ++ showObj o

foldlCons :: (Object -> Object -> SchemeM Object) -> Object -> Object ->
	SchemeM Object
foldlCons _ o0 ONil = return o0
foldlCons f o0 (OCons a d) = flip (foldlCons f) d =<< f o0 a
foldlCons _ _ _ = throwError "foldlCons: bad"

zipWithCons :: (Object -> Object -> SchemeM Object) -> Object -> Object ->
	SchemeM Object
zipWithCons f (OCons a d) (OCons a' d') = OCons <$> f a a' <*> zipWithCons f d d'
zipWithCons _ ONil _ = return ONil
zipWithCons _ _ ONil = return ONil
zipWithCons _ _ _ = throwError "zipWithCons: bad"

mapCons :: (Object -> SchemeM Object) -> Object -> SchemeM Object
mapCons _ ONil = return ONil
mapCons f (OCons a d) = OCons <$> f a <*> mapCons f d
mapCons _ o = throwError $
	"*** ERROR: proper list required for function application or macro use: "
	++ showObj o
