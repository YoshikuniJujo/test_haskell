import Control.Monad.Cont

data VE e a = Val a | E (Reader e (VE e a))

newtype Reader e v = Reader (e -> v)

ask :: Cont (VE e a) e
ask = cont $ E . Reader

runReader :: Cont (VE e a) a -> e -> a
runReader m e = case runCont m Val of
	Val x -> x
	E (Reader u) -> case u e of
		Val y -> y
		E (Reader v) -> undefined
