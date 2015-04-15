{-# LANGUAGE PackageImports #-}

import Control.Applicative
import Control.Arrow
import "monads-tf" Control.Monad.State
import Data.List
import Data.Ratio
import System.IO.Unsafe
import Debug.Trace

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

type Circuit a = (Int, [[Wire]]) -> (a, (Int, [[Wire]]))

createResistor :: [[Wire]] -> Resistor
createResistor wss
	| Just i <- findIndex (elem St) wss = fst $ crRg Nothing (i, wss)
	| otherwise = error "bad"

crRg :: Maybe Int -> Circuit Resistor
crRg b (i, wss) = case popWire (i, wss) of
	(St, (i', wss')) -> crRg b (i', wss')
	(Jn, (i', wss')) -> crRg b (i', wss')
	(UpSt, (i', wss')) -> crRg b (i', wss')
	(DnSt, (i', wss')) -> crRg b (i', wss')
	(Rs r, (i', wss')) -> let (n, (i'', wss'')) = crRg b (i', wss') in
		(Resistor (r % 1) :--> n, (i'', wss''))
	(Ed, (i', wss')) -> if b == Nothing then (NoResistor, (i', wss')) else error "bad"
	(Br, (i', wss')) -> let
		(u, (i'', wss'')) = crRg (Just i') (i' - 1, wss')
		(d, (i''', wss''')) = crRg (Just i') (i'' + 1, wss'')
		(n, (i'''', wss'''')) = crRg b (i''', wss''') in
		(u :-|- d :--> n, (i'''', wss''''))
	(Dn, (i', wss')) -> let i'' = i' + 1 in
		case b of
			Just bs -> if i'' == bs
				then (NoResistor, (i'', wss'))
				else crRg b (i'', wss')
			_ -> error "bad"
	(Up, (i', wss')) -> let i'' = i' - 1 in
		case b of
			Just bs -> if i'' == bs
				then (NoResistor, (i'', wss'))
				else crRg b (i'', wss')
			_ -> error "bad"

{-
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
		-}

popWire :: Circuit Wire
popWire (i, wss) = let (w, wss') = popW i wss in (w, (i, wss'))
upWire, downWire :: Circuit ()
upWire (i, wss) = ((), (i - 1, wss))
downWire (i, wss) = ((), (i + 1, wss))

popW :: Int -> [[Wire]] -> (Wire, [[Wire]])
popW n x | n < 0 = error $ show n ++ " " ++ show x
popW 0 ((w : ws) : wss) = (w, ws : wss)
popW n (ws : wss) = (ws :) `second` popW (n - 1) wss
popW x y = error $ show x ++ " " ++ show y
