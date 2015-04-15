import Control.Applicative
import Control.Arrow
import Data.List
import Data.Ratio
import System.IO.Unsafe

sample, sample2 :: [String]
sample = lines . unsafePerformIO $ readFile "sampleCircuit.txt"
sample2 = lines . unsafePerformIO $ readFile "sampleCircuit2.txt"

data Register
	= Single (Ratio Int)
	| Series Register Register
	| Parallel Register Register
	| NoRegister
	deriving (Show, Eq)

data Wire = Rg Register | St | Ed | Br | Jn | Up | Dn deriving (Show, Eq)

register :: Register -> Ratio Int
register (Single n) = n
register (Series r1 r2) = register r1 + register r2
register (Parallel r1 r2) =
	(register r1 * register r2) / (register r1 + register r2)
register NoRegister = 0

-- readCircuit :: Int -> [String] -> Register
-- readCircuit n ss =

readCircuit1 :: Bool -> Int -> String -> [(Int, Wire)]
readCircuit1 e _ "" = []
readCircuit1 False p ('@' : s) = (p, St) : readCircuit1 True (p + 1) s
readCircuit1 True p ('@' : s) = (p, Ed) : readCircuit1 False (p + 1) s
readCircuit1 e p ('-' : s) = readCircuit1 e (p + 1) s
readCircuit1 e p (' ' : s) = readCircuit1 e (p + 1) s
readCircuit1 e p ('<' : s) = (p, Br) : readCircuit1 e (p + 1) s
readCircuit1 e p ('>' : s) = (p, Jn) : readCircuit1 e (p + 1) s
readCircuit1 e p ('/' : s) = (p, Up) : readCircuit1 e (p + 1) s
readCircuit1 e p ('\\' : s) = (p, Dn) : readCircuit1 e (p + 1) s
readCircuit1 e p s = let (n, r) = readRegister s in
	(p, Rg r) : readCircuit1 e (p + n) (drop n s)

readRegister :: String -> (Int, Register)
readRegister ('|' : s) = case span (/= '|') s of
	(n, '|' : r) -> (2 + length n, Single . (% 1) $ read n)
	_ -> error "bad"

readWire :: String -> (Int, Bool)
readWire ('-' : s) = first (+ 1) $ readWire s
readWire ('@' : s) = (1, False)

createRegister :: [[(Int, Wire)]] -> Register
createRegister ws = let (i, ws') = checkStart ws in crRg i 0 Nothing ws'

checkStart :: [[(Int, Wire)]] -> (Int, [[(Int, Wire)]])
checkStart (wa1@((0, St) : ws) : wss) = (0, ws : wss)
checkStart (ws : wss) = (+ 1) *** (ws :) $ checkStart wss

crRg :: Int -> Int -> Maybe Int -> [[(Int, Wire)]] -> Register
crRg y s e wss = case wss !! y of
	(n, Rg r) : ws -> Series r . crRg y n e $ map (dropWhile ((<= n) . fst)) wss
	(n, Br) : (n2, Jn) : ws -> Series (
		Parallel
			(crRg (y - 1) (n + 1) (Just n2) wss)
			(crRg (y + 1) (n + 1) (Just n2) wss))
		. crRg y (n2 + 1) e $ map (dropWhile ((<= n2 + 1) . fst)) wss
	(n, Up) : ws -> if e == Just (n + 1)
		then NoRegister
		else crRg y n e $ map (dropWhile ((<= n) . fst)) wss
	(n, Dn) : ws -> if e == Just (n + 1)
		then NoRegister
		else crRg y n e $ map (dropWhile ((<= n) . fst)) wss
	[(_, Ed)] -> NoRegister
