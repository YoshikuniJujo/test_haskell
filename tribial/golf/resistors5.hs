{-# LANGUAGE PackageImports #-}

import Control.Applicative
import Control.Arrow
import "monads-tf" Control.Monad.State
import Data.List
import Data.Ratio
import System.IO.Unsafe
import Debug.Trace

main :: IO ()
main = interact $ show . numerator . resistor . createResistor2 . lines

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

setElem :: [a] -> Int -> a -> [a]
setElem xs i x = take i xs ++ [x] ++ drop (i + 1) xs

createResistor2 :: [String] -> Resistor
createResistor2 wss
	| Just i <- findIndex (elem '@') wss = fst $ crRg2 Nothing (i, wss)

crRg2 :: Maybe Int -> (Int, [String]) -> (Resistor, (Int, [String]))
crRg2 b (i, wss) = case wss !! i of
	'@' : '-' : ws -> crRg2 b (i, setElem wss i ws)
	'/' : '-' : ws -> crRg2 b (i, setElem wss i ws)
	'\\': '-' : ws -> crRg2 b (i, setElem wss i ws)
	'-' : '@' : ws -> (NoResistor, (i, setElem wss i ws))
	'-' : ws -> crRg2 b (i, setElem wss i ws)
	' ' : ws -> crRg2 b (i, setElem wss i ws)
	'>' : ws -> crRg2 b (i, setElem wss i ws)
	'|' : ws -> let
		(n, _ : rst) = span (/= '|') ws
		(r, s) = crRg2 b (i, setElem wss i rst) in
		(Resistor (read n % 1) :--> r, s)
	'<' : ws -> let
		(u, (_, ws')) = crRg2 (Just i) (i - 1, setElem wss i ws)
		(d, (_, ws'')) = crRg2 (Just i) (i + 1, ws')
		(n, (_, ws''')) = crRg2 b (i, ws'') in
		(u :-|- d :--> n, (i, ws'''))
	'\\' : ws -> case b of
		Just bs -> if i + 1 == bs
			then (NoResistor, (i + 1, setElem wss i ws))
			else crRg2 b (i + 1, setElem wss i ws)
	'/' : ws -> case b of
		Just bs -> if i - 1 == bs
			then (NoResistor, (i - 1, setElem wss i ws))
			else crRg2 b (i - 1, setElem wss i ws)
	ws -> error $ show ws
