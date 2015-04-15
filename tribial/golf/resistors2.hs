{-# LANGUAGE PackageImports #-}

import Control.Arrow
import "monads-tf" Control.Monad.State
import Data.List
import Data.Ratio
import System.IO.Unsafe

sample, sample2 :: [String]
sample = lines . unsafePerformIO $ readFile "sampleCircuit.txt"
sample2 = lines . unsafePerformIO $ readFile "sampleCircuit2.txt"
sample3 = lines . unsafePerformIO $ readFile "sampleCircuit3.txt"
sample5 = lines . unsafePerformIO $ readFile "sampleCircuit5.txt"
sample6 = lines . unsafePerformIO $ readFile "sampleCircuit6.txt"
test = lines . unsafePerformIO $ readFile "testCircuit.txt"
test2 = lines . unsafePerformIO $ readFile "testCircuit2.txt"

data Resistor
	= Resistor (Ratio Int)
	| Resistor :--> Resistor
	| Resistor :-|- Resistor
	| NoResistor
	deriving Show

resistor :: Resistor -> Ratio Int
resistor (Resistor r) = r
resistor (r1 :--> r2) = resistor r1 + resistor r2
resistor (r1 :-|- r2) = resistor r1 * resistor r2 / (resistor r1 + resistor r2)
resistor NoResistor = 0

data Wire
	= St | Ed | UpSt | DnEd | DnSt | UpEd
	| Br | Jn | Up | Dn | Rg Int deriving (Show, Eq)

readWires :: String -> [Wire]
readWires "" = []
readWires ('@' : '-' : s) = St : readWires s
readWires ('/' : '-' : s) = UpSt : readWires s
readWires ('\\': '-' : s) = DnSt : readWires s
readWires ('-' : '@' : s) = Ed : readWires s
readWires ('-' : '/' : s) = UpEd : readWires s
readWires ('-' : '\\': s) = DnEd : readWires s
readWires ('<' : s) = Br : readWires s
readWires ('>' : s) = Jn : readWires s
readWires ('/' : s) = Up : readWires s
readWires ('\\': s) = Dn : readWires s
readWires ('-' : s) = readWires s
readWires (' ' : s) = readWires s
readWires ('|' : s) =
	let (n, '|' : r) = span (/= '|') s in Rg (read n) : readWires r

createResistor :: [[Wire]] -> Resistor
createResistor wss
	| Just i <- findIndex (elem St) wss = fst $ crRg `runState` (i, wss)
	| otherwise = error "bad"

type Circuit = State (Int, [[Wire]])

crRg :: Circuit Resistor
crRg = do
	ws <- popWire
	case ws of
		St -> crRg
		UpSt -> crRg
		DnSt -> crRg
		Rg r -> do
			n <- crRg
			return $ Resistor (r % 1) :--> n
		Ed -> return NoResistor
		UpEd -> upWire >> crEd >> return NoResistor
		DnEd -> downWire >> crEd >> return NoResistor
		Br -> do
			i <- gets fst
			u <- upWire >> crRg
			modify (first (const i))
			d <- downWire >> crRg
			modify (first (const i))
			n <- crRg
			return $ u :-|- d :--> n
		Up -> upWire >> crRg
		Dn -> downWire >> crRg
		Jn -> crRg

crEd = do
	ws <- popWire
	case ws of
		Up -> upWire
		Dn -> downWire

popWire :: Circuit Wire
popWire = do
	(i, wss) <- get
	let (w, wss') = popW i wss
	put (i, wss')
	return w

upWire, downWire :: Circuit ()
upWire = modify (first pred)
downWire = modify (first succ)

popW :: Int -> [[Wire]] -> (Wire, [[Wire]])
popW 0 ((w : ws) : wss) = (w, ws : wss)
popW n (ws : wss) = (ws :) `second` popW (n - 1) wss
popW i wss = error $ show i ++ " " ++ show wss
