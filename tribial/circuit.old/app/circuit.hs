{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad.State
import Control.Monad.Writer

import Gates

main :: IO ()
main = putStrLn "Slozsoft"

data Environment = Env {
	idn :: Id,
	gates :: [Gates 2 2]
	}

succId :: Environment -> Environment
succId e = e { idn = idn e + 1 }

addGate :: Gates 2 2 -> Environment -> Environment
addGate g e = e { gates = g : gates e }

type CircuitMonad = WriterT String (State Environment)

runCircuitMonad :: CircuitMonad a -> (a, String)
runCircuitMonad = (`evalState` Env 0 []) . runWriterT

getModify :: MonadState m =>
	(StateType m -> StateType m) -> (StateType m -> a) -> m a
getModify f g = (>>) <$> (put . f) <*> return . g =<< get

newId :: CircuitMonad Id
newId = getModify succId idn

createAndGate :: CircuitMonad (Id, Id, Id)
createAndGate = do
	ws@(in1, in2, out) <-
		(\[i1, i2, o] -> (i1, i2, o)) <$> replicateM 3 newId
	modify . addGate $ mkAndGate in1 in2 out
	return ws

createNotGate :: CircuitMonad (Id, Id)
createNotGate = do
	ws@(in1, out) <- (\[i, o] -> (i, o)) <$> replicateM 2 newId
	modify . addGate $ mkNotGate in1 out
	return ws

setBit :: Id -> Bit -> CircuitMonad ()
setBit w b = do
	tell $ "set " ++ show w ++ " " ++
		(case b of O -> "off"; I -> "on") ++ ".\n"
