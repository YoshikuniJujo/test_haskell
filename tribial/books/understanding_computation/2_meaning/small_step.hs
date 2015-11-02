-- import Control.Applicative
import Data.Maybe
import Data.List

data Expression
	= N Integer | Expression :+: Expression | Expression :*: Expression
	| B Bool | Expression :<: Expression
	deriving Show

reduce :: Expression -> Maybe Expression
reduce (N l :+: N r) = Just . N $ l + r
reduce (N l :*: N r) = Just . N $ l * r
reduce (N l :<: N r) = Just . B $ l < r
reduce (l :+: r)
	| Just l' <- reduce l = Just $ l' :+: r
	| Just r' <- reduce r = Just $ l :+: r'
reduce (l :*: r)
	| Just l' <- reduce l = Just $ l':*:  r
	| Just r' <- reduce r = Just $ l :*: r'
reduce (l :<: r)
	| Just l' <- reduce l = Just $ l' :<: r
	| Just r' <- reduce r = Just $ l :<: r'
reduce _ = Nothing

run :: Expression -> [Expression]
run = map fromJust . takeWhile isJust . iterate (>>= reduce) . Just
