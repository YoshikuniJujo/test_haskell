module Object (
	SchemeM,
	Object(..),
	showObj,

	Environment, mkInitEnv,
	EnvT, runEnvT,
	define, getValue, getEID, newEnv, popEnv,
	throwError, catchError,
	EID,
) where

import Env

import Data.Ratio
import Data.Maybe

type SchemeM = EnvT Object IO

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
	| OSubr String (Object -> SchemeM Object)
	| OSyntax String (Object -> SchemeM Object)
	| OClosure (Maybe String) EID Object Object

showObj :: Object -> String
showObj (OInt i) = show i
showObj (ODouble d) = show d
showObj (ORational r) = show (numerator r) ++ "/" ++ show (denominator r)
showObj (OString s) = show s
showObj (OVar v) = v
showObj c@(OCons _ _) = showCons False c
showObj ONil = "()"
showObj (OBool True) = "#t"
showObj (OBool False) = "#f"
showObj OUndef = "#<undef>"
showObj (OSubr n _) = "#<subr " ++ n ++ ">"
showObj (OSyntax n _) = "#<syntax " ++ n ++ ">"
showObj (OClosure n _ _ _) = "#<closure " ++ fromMaybe "#f" n ++ ">"

showCons :: Bool -> Object -> String
showCons l (OCons a d) = (if l then id else ("(" ++ ) . (++ ")")) $
	case d of
		OCons _ _ -> showObj a ++ " " ++ showCons True d
		ONil -> showObj a
		_ -> showObj a ++ " . " ++ showObj d
showCons l ONil = if l then "" else "()"
showCons _ _ = error "not cons"
