{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import MyEff
import MyEff.Lift
import MyEff.Writer
import MyEff.State
import MyEff.Operational
import TypeLevel

data Jail a where
	Print :: String -> Jail ()
	Scan :: Jail String

prog :: Member (Program Jail) es => Eff es ()
prog = do
	singleton $ Print "getting input..."
	str <- singleton Scan
	singleton $ Print "ok"
	singleton . Print $ "the input is " ++ str

adventIO :: (BaseLift (Lift IO) es) => Jail a -> Eff es a
adventIO (Print a) = lift $ putStrLn a
adventIO Scan = lift getLine

adventPure :: (Member (Writer String) es, Member (State [String]) es) =>
	Jail a -> Eff es a
adventPure (Print a) = tell $ a ++ "\n"
adventPure Scan = do
	x <- get
	case x of
		[] -> return []
		y : ys -> put ys >> return y

testIO :: IO ()
testIO = runLift $ runProgram adventIO
	(prog :: Eff (Program Jail :> Lift IO :> Base) ())

testPure :: [String] -> (((), [String]), String)
testPure is = run . runWriter . (`runState` is) $ runProgram adventPure
	(prog :: Eff (
		Program Jail :> State [String] :> Writer String :> Base ) ())
