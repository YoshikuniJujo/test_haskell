-- import Control.Applicative
import Data.Maybe
-- import Data.List
import qualified Data.Map as Map

data Statement
	= DoNothing
	| Assign String Expression
	deriving Show

reduceSt :: Statement -> Environment -> Maybe (Statement, Environment)
reduceSt DoNothing _ = Nothing
reduceSt (Assign v ex) env
	| Just ex' <- reduceEx ex env = Just (Assign v ex', env)
	| otherwise = Just (DoNothing, Map.insert v ex env)

runSt :: Statement -> Environment -> [(Statement, Environment)]
runSt ex env = map fromJust . takeWhile isJust
	. iterate (>>= uncurry reduceSt) $ Just (ex, env)

data Expression
	= N Integer | Expression :+: Expression | Expression :*: Expression
	| B Bool | Expression :<: Expression
	| V String
	deriving Show

type Environment = Map.Map String Expression

reduceEx :: Expression -> Environment -> Maybe Expression
reduceEx (N l :+: N r) _ = Just . N $ l + r
reduceEx (N l :*: N r) _ = Just . N $ l * r
reduceEx (N l :<: N r) _ = Just . B $ l < r
reduceEx (l :+: r) env
	| Just l' <- reduceEx l env = Just $ l' :+: r
	| Just r' <- reduceEx r env = Just $ l :+: r'
reduceEx (l :*: r) env
	| Just l' <- reduceEx l env = Just $ l':*:  r
	| Just r' <- reduceEx r env = Just $ l :*: r'
reduceEx (l :<: r) env
	| Just l' <- reduceEx l env = Just $ l' :<: r
	| Just r' <- reduceEx r env = Just $ l :<: r'
reduceEx (V v) env = Map.lookup v env
reduceEx _ _ = Nothing

runEx :: Expression -> Environment -> [Expression]
runEx ex env = map fromJust . takeWhile isJust
	. iterate (>>= (`reduceEx` env)) $ Just ex
