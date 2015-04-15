{-# LANGUAGE PackageImports #-}

import Control.Applicative
import Control.Arrow
import "monads-tf" Control.Monad.State
import Data.List
import Data.Ratio
import System.IO.Unsafe

main :: IO ()
main = interact $ show . numerator . resistor . createResistor . map readWires . lines

sample, sample2 :: [String]
sample = lines . unsafePerformIO $ readFile "sampleCircuit.txt"
sample2 = lines . unsafePerformIO $ readFile "sampleCircuit2.txt"
sample3 = lines . unsafePerformIO $ readFile "sampleCircuit3.txt"
sample5 = lines . unsafePerformIO $ readFile "sampleCircuit5.txt"
sample6 = lines . unsafePerformIO $ readFile "sampleCircuit6.txt"
sample7 = lines . unsafePerformIO $ readFile "sampleCircuit7.txt"
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
	= St | Ed | UpSt | DnSt | Up | Dn | Br | Jn
	| Rs Int
	deriving (Show, Eq)

readWires :: String -> [Wire]
readWires "" = []
readWires ('@' : '-' : s) = St : readWires s
readWires ('-' : '@' : s) = Ed : readWires s
readWires ('/' : '-' : s) = UpSt : readWires s
readWires ('\\': '-' : s) = DnSt : readWires s
readWires ('/' : s) = Up : readWires s
readWires ('\\': s) = Dn : readWires s
readWires ('<' : s) = Br : readWires s
readWires ('>' : s) = Jn : readWires s
readWires ('|' : s) =
	let (n, '|' : r) = span (/= '|') s in Rs (read n) : readWires r
readWires ('-' : s) = readWires s
readWires (' ' : s) = readWires s

type Circuit = State (Int, [[Wire]])

createResistor :: [[Wire]] -> Resistor
createResistor wss
	| Just i <- findIndex (elem St) wss = fst $ crRg Nothing `runState` (i, wss)
	| otherwise = error "bad"

crRg :: Maybe Int -> Circuit Resistor
crRg b = do
	w <- popWire
	case w of
		St -> crRg b
		UpSt -> crRg b
		DnSt -> crRg b
		Ed -> if b == Nothing then return NoResistor else error "bad"
		Dn -> do
			downWire
			i <- gets fst
			case b of
				Just bs -> if i == bs
					then return NoResistor else crRg b
				_ -> error "bad"
		Up -> do
			upWire
			i <- gets fst
			case b of
				Just bs -> if i == bs
					then return NoResistor else crRg b
				_ -> error "bad"
		Br -> do
			i <- gets fst
			u <- upWire >> crRg (Just i)
			d <- downWire >> crRg (Just i)
			Jn <- popWire
			n <- crRg b
			return $ u :-|- d :--> n
		Rs r -> (Resistor (r % 1) :-->) <$> crRg b
		_ -> error $ show w

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
