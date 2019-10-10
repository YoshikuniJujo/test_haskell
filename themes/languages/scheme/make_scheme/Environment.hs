module Environment (
	Env, Scope,
	Environment.fromList,
	Environment.lookup, Environment.insert,
	set,
	local, exit, getScope, setScope, global,

	Symbol, Value(..), Error(..), toDouble, negateValue,
	) where

import Control.Applicative
import Data.Maybe
import System.Random

import qualified Data.Map as M

data Env = Env {
	scopeNow :: EnvId,
	fromEnv :: M.Map EnvId LocalEnv
	} deriving Show

data LocalEnv = LocalEnv {
	fromLocalEnv :: M.Map Symbol Value,
	outerScope :: Maybe EnvId
	} deriving Show

data Scope = Scope EnvId deriving Show

global :: Scope
global = Scope 0

type EnvId = Int

type Symbol = String

envNow :: Env -> LocalEnv
envNow e = fromJust $ M.lookup (scopeNow e) (fromEnv e)

outer :: Env -> Either Error Env
outer e = maybe (Left $ Error "outer: yet") Right $ do
	le <- M.lookup (scopeNow e) (fromEnv e)
	(`Env` fromEnv e) <$> outerScope le

fromList :: [(Symbol, Value)] -> Env
fromList lst = Env {
	scopeNow = 0,
	fromEnv = M.singleton 0 LocalEnv {
		fromLocalEnv = M.fromList lst,
		outerScope = Nothing } }

lookup :: Symbol -> Env -> Either Error Value
lookup s e = case M.lookup s (fromLocalEnv $ envNow e) of
	Just v -> Right v
	_ -> do	oe <- outer e
		Environment.lookup s oe

insert :: Symbol -> Value -> Env -> Env
insert s v e = Env (scopeNow e) $ M.insert
	(scopeNow e)
	(LocalEnv (M.insert s v $ fromLocalEnv en) (outerScope en))
	(fromEnv e)
	where
	en = envNow e

varIsIn :: Symbol -> Env -> Either Error (EnvId, LocalEnv)
varIsIn s e = case M.lookup s $ fromLocalEnv le of
	Just _ -> Right (scopeNow e, le)
	_ -> do	oe <- outer e
		varIsIn s oe
	where
	le = envNow e

sampleEnv0 :: Env
sampleEnv0 = Environment.fromList [
	("hello", Symbol "world"),
	("good-bye", Symbol "you") ]

set :: Symbol -> Value -> Env -> Either Error Env
set s v e = do
	(eid, le) <- varIsIn s e
	return $ e {
		fromEnv = M.insert eid
			le { fromLocalEnv = M.insert s v (fromLocalEnv le) } 
			(fromEnv e) }

local :: Env -> Env
local e = let (mx, _) = M.findMax (fromEnv e) in Env {
	scopeNow = mx + 1,
	fromEnv = M.insert
		(mx + 1)
		(LocalEnv M.empty . Just $ scopeNow e)
		(fromEnv e)
	}

exit :: Env -> Env
exit e@Env { scopeNow = 0 } = e
exit e = e {
	scopeNow = fromJust . outerScope
		. fromJust $ M.lookup (scopeNow e) (fromEnv e)
	}

sampleEnv1 :: Env
sampleEnv1 = Environment.insert "yamada" (Symbol "takao") $ local sampleEnv0

getScope :: Env -> Scope
getScope = Scope . scopeNow

setScope :: Scope -> Env -> Env
setScope (Scope sn) e = e { scopeNow = sn }

data Value
	= Undef | Nil | Bool Bool | Symbol Symbol
	| Integer Rational | Double Double | String String
	| Cons Value Value
	| DoExit | Display String
	| Subroutine Symbol (Value -> Env -> Either Error ((String, Value), Env))
	| Syntax Symbol (Value -> Env -> Either Error ((String, Value), Env))
	| Closure Symbol Scope [Symbol] Value
	| Define Symbol Value
	| RandomSeed StdGen

instance Show Value where
	show (Symbol s) = s
	show (Integer i) = show i
	show (Double d) = show d
	show (Cons v1 v2) = "(" ++ show v1 ++ " . " ++ show v2 ++ ")"
	show Nil = "()"
	show _ = "instance Show Value: yet"

data Error
	= Exit
	| Error String
	deriving Show

toDouble :: Value -> Maybe Double
toDouble (Integer r) = Just $ realToFrac r
toDouble (Double d) = Just d
toDouble _ = Nothing

negateValue :: Value -> Value
negateValue (Integer r) = Integer $ - r
negateValue (Double d) = Double $ - d
negateValue v = v
