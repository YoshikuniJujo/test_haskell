module Environment (
	Env, Scope,
	Environment.fromList, Environment.lookup, Environment.insert, set,
	local, exit, getScope, setScope,

	Symbol, Value(..), Error(..), toDouble, negateValue,
	) where

import Control.Applicative
import System.Random

import qualified Data.Map as M

data Env = Env { fromEnv :: [M.Map Symbol Value] } deriving Show

data Scope = Scope deriving Show

type Symbol = String

fromList :: [(Symbol, Value)] -> Env
fromList = Env . (: []) . M.fromList

insert :: Symbol -> Value -> Env -> Env
insert s v = Env . heading (M.insert s v) . fromEnv
	where
	heading :: (a -> a) -> [a] -> [a]
	heading f (x : xs) = f x : xs
	heading _ _ = []

set :: Symbol -> Value -> Env -> Either Error Env
set s v (Env (e : es)) = maybe
	(Env . (e :) . fromEnv <$> set s v (Env es))
	(const . Right . Env $ M.insert s v e : es) $ M.lookup s e
set _ _ _ = Left $ Error "set: yet"

lookup :: Symbol -> Env -> Maybe Value
lookup s (Env (e : es)) = maybe (Environment.lookup s $ Env es) Just $ M.lookup s e
lookup _ _ = Nothing

local :: Env -> Env
local = Env . (M.empty :) . fromEnv

exit :: Env -> Env
exit (Env (_ : es@(_ : _))) = Env es
exit e@(Env _) = e

getScope :: Env -> Scope
getScope _ = Scope

setScope :: Scope -> Env -> Env
setScope _ e = e

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
