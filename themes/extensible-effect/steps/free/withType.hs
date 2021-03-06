{-# LANGUAGE ExistentialQuantification, DeriveFunctor #-}
{-# LANGUAGE KindSignatures, DataKinds, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE RankNTypes, GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

import Control.Arrow
import Data.Typeable
import Data.Monoid

import TypeLevel
import Free
import Cast

data Union (es :: Effs) a = forall t . (Functor t, Typeable t) => U (t a)

instance Functor (Union es) where
	fmap f (U ta) = U $ fmap f ta

toUnion :: (Functor t, Typeable t) => t a -> Union es a
toUnion = U

fromUnion :: (Functor t, Typeable t) => Union es a -> Maybe (t a)
fromUnion (U ta) = cast1 ta

castUnion :: Union es a -> Union es' a
castUnion (U ta) = U ta

run :: Free (Union Emp) a -> a
run (Pure x) = x
run _ = error "never occur"

newtype Reader r w = Reader (r -> w) deriving Functor

ask :: (Member (Reader r) es, Typeable r) => Free (Union es) r
ask = Free . U $ Reader Pure

runReader :: Typeable r =>
	Free (Union (Reader r :> es)) a -> r -> Free (Union es) a
runReader f r = case f of
	Pure x -> Pure x
	Free u -> case fromUnion u of
		Just (Reader f') -> runReader (f' r) r
		Nothing -> Free . fmap (`runReader` r) $ castUnion u

data Writer wr w = Writer wr w deriving Functor

tell :: (Member (Writer wr) es, Typeable wr) => wr -> Free (Union es) ()
tell wr = Free . U $ Writer wr (Pure ())

runWriter :: (Monoid wr, Typeable wr) =>
	Free (Union (Writer wr :> es)) a -> Free (Union es) (a, wr)
runWriter f = case f of
	Pure x -> Pure (x, mempty)
	Free u -> case fromUnion u of
		Just (Writer wr f') -> second (wr <>) <$> runWriter f'
		Nothing -> Free . fmap runWriter $ castUnion u

data State s w = State (s -> s) (s -> w) deriving Functor

getModify :: (Member (State s) es, Typeable s) => (s -> s) -> Free (Union es) s
getModify f = Free . U $ State f Pure

get :: (Member (State s) es, Typeable s) => Free (Union es) s
get = getModify id

modify :: (Member (State s) es, Typeable s) => (s -> s) -> Free (Union es) ()
modify = (>> return ()) . getModify

put :: (Member (State s) es, Typeable s) => s -> Free (Union es) ()
put = modify . const

runState :: Typeable s =>
	Free (Union (State s :> es)) a -> s -> Free (Union es) (a, s)
runState f s = case f of
	Pure x -> Pure (x, s)
	Free u -> case fromUnion u of
		Just (State m f') -> runState (f' s) (m s)
		Nothing -> Free . fmap (`runState` s) $ castUnion u

lift :: (Typeable m, BaseLift (Lift m) es) => m a -> Free (Union es) a
lift m = Free . U $ Lift m Pure

runLift :: (Monad m, Typeable m) => Free (Union (Lift m :> Emp)) a -> m a
runLift f = case f of
	Pure x -> return x
	Free u -> case fromUnion u of
		Just (Lift m' k) -> m' >>= runLift . k
		Nothing -> error "cannot occur"

data Program instr v = forall a . Program (instr a) (a -> v)

instance Functor (Program instr) where
	fmap f (Program instr k) = Program instr (f . k)

singleton :: (Member (Program instr) es, Typeable instr) =>
	instr a -> Free (Union es) a
singleton i = Free . U $ Program i Pure

runProgram :: Typeable f => (forall x . f x -> Free (Union es) x) ->
	Free (Union (Program f :> es)) a -> Free (Union es) a
runProgram advent f = case f of
	Pure x -> Pure x
	Free u -> case fromUnion u of
		Just (Program instr k) ->
			advent instr >>= runProgram advent . k
		Nothing -> Free . fmap (runProgram advent) $ castUnion u

testRS :: Free (Union (Reader Char :> State Integer :> Emp)) (Char, Integer)
testRS = do
	r <- ask
	modify (+ (15 :: Integer))
	s <- get
	modify (+ (123 :: Integer))
	return (r, s)

testRW :: Free (Union (Reader Char :> Writer String :> Emp)) Char
testRW = do
	r <- ask
	tell "hige"
	return r

data Jail a where
	Print :: String -> Jail ()
	Scan :: Jail String

prog :: Member (Program Jail) es => Free (Union es) ()
prog = do
	singleton $ Print "getting input..."
	str <- singleton Scan
	singleton $ Print "ok"
	singleton $ Print ("the input is " ++ str)

adventIO :: (BaseLift (Lift IO) es) => Jail a -> Free (Union es) a
adventIO (Print a) = lift $ putStrLn a
adventIO Scan = lift getLine

adventPure :: (Member (Writer String) es, Member (State [String]) es) =>
	Jail a -> Free (Union es) a
adventPure (Print a) = tell (a ++ "\n")
adventPure Scan = do
	x <- get
	case x of
		[] -> return []
		y : ys -> put ys >> return y

testIO :: IO ()
testIO = runLift $ runProgram adventIO
	(prog :: Free (Union (Program Jail :> Lift IO :> Emp)) ())

testPure :: [String] -> (((), [String]), String)
testPure is = run . runWriter . (`runState` is) $ runProgram adventPure
	(prog :: Free (Union (
		Program Jail :> State [String] :> Writer String :> Emp )) ())
