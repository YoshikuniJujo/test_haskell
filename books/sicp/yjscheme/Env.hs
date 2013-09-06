{-# LANGUAGE PackageImports, TupleSections #-}

module Env (
	SchemeM,
	Env, EID, topEID, EKey(..),
	Object(..), showObj,

	defineVar, nowEnv, newEnv, intoEnv,
) where

import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Maybe
import Data.Ratio
import Control.Applicative

type SchemeM = EnvM (ErrorT Err IO)
type Err = String

type EnvM = StateT ((EID, Maybe (EID, Env)), Env)
type Env = [(EKey, Object)]
data EKey = EVar String | EID EID deriving (Eq, Show)
type EID = Int
topEID :: EID
topEID = 0

data Object
	= OInt Integer
	| ORational Rational
	| ODouble Double
	| OString String
	| OVar String
	| OCons Object Object
	| ONil
	| OUndef
	| OBool Bool
	| OSubr String (Object -> SchemeM Object)
	| OSyntax String (Object -> SchemeM Object)
	| OEnv EID Env
	| OClosure (Maybe String) EID Object Object

instance Show Object where
	show (ODouble d) = "(ODouble " ++ show d ++ ")"
	show _ = "show Object: yet"

showObj :: Object -> String
showObj (OInt i) = show i
showObj (ORational r) = show (numerator r) ++ "/" ++ show (denominator r)
showObj (ODouble d) = show d
showObj (OString s) = show s
showObj (OVar v) = v
showObj (OSubr n _) = "#<subr " ++ n ++ ">"
showObj (OSyntax n _) = "#<syntax " ++ n ++ ">"
showObj (OCons (OVar "quote") (OCons a ONil)) = "'" ++ showObj a
showObj c@(OCons _ _) = showCons False c
showObj ONil = "()"
showObj (OBool True) = "#t"
showObj (OBool False) = "#f"
showObj OUndef = "#<undef>"
showObj (OEnv eid _) = "#<env " ++ show eid ++ ">"
showObj (OClosure n _ _ _) = "#<closure " ++ fromMaybe "#f" n ++ ">"

showCons :: Bool -> Object -> String
showCons l (OCons a d) = (if l then id else ("(" ++) . (++ ")")) $
	case d of
		OCons _ _ -> showObj a ++ " " ++ showCons True d
		ONil -> showObj a
		_ -> showObj a ++ " . " ++ showObj d
showCons l ONil = if l then "" else "()"
showCons _ _ = error "not cons"

defineVar :: String -> Object -> SchemeM ()
defineVar var val = do
	((maxID, mhere), tenv) <- get
	case mhere of
		Just (hid, henv) -> put ((maxID, Just (hid, (EVar var, val) : henv)), tenv)
		Nothing -> put ((maxID, mhere), (EVar var, val) : tenv)

nowEnv :: SchemeM EID
nowEnv = do
	((_, here), _) <- get
	return $ case here of
		Just (eid, _) -> eid
		_ -> 0

newEnv :: SchemeM EID
newEnv = do
	((maxID, here), tenv) <- get
	let	newID = succ maxID
	put ((newID, here), (EID newID, OEnv newID $ maybe [] snd here) : tenv)
	return $ newID

intoEnv :: EID -> SchemeM ()
intoEnv eid = do
	((maxID, here), tenv) <- get
	case here of
		Just (oeid, oenv) ->
			put ((maxID, (eid ,) . (\(OEnv _ e) -> e) <$>
					lookup (EID eid) tenv),
				(EID oeid, OEnv oeid oenv) : tenv)
		_ -> put ((maxID, (eid ,) . (\(OEnv _ e) -> e) <$>
					lookup (EID eid) tenv), tenv)
