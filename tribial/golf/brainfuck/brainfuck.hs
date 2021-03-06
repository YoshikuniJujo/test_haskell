import Data.Char
import System.Environment
import Numeric

main :: IO ()
main = do
	fn : _ <- getArgs
	src <- readFile fn
	ms <- brainfuck 0 [] ("", src)
	putStrLn ""
	putStrLn ""
	putStrLn . unwords $ map (two . flip showHex "") ms

two :: String -> String
two s = replicate (2 - length s) '0' ++ s

brainfuck :: Int -> [Int] -> (String, String) -> IO [Int]
brainfuck _ ms (_, "") = return ms
brainfuck ptr ms (pre, '>' : src) = brainfuck (succ ptr) ms ('>' : pre, src)
brainfuck ptr ms (pre, '<' : src) = brainfuck (pred ptr) ms ('<' : pre, src)
brainfuck ptr ms (pre, '+' : src) = brainfuck ptr (inc ptr ms) ('+' : pre, src)
brainfuck ptr ms (pre, '-' : src) = brainfuck ptr (dec ptr ms) ('-' : pre, src)
brainfuck ptr ms (pre, '.' : src) = do
	putChar (chr $ ms !! ptr)
	brainfuck ptr ms ('.' : pre, src)
brainfuck ptr ms (pre, ',' : src) = do
	c <- getChar
	brainfuck ptr (modify (const $ ord c) ptr ms) (',' : pre, src)
brainfuck ptr ms (pre, '[' : src)
	| length ms < ptr + 1 || ms !! ptr == 0 =
		brainfuck ptr ms . bracket 1 $ ('[' : pre, src)
	| otherwise = brainfuck ptr ms ('[' : pre, src)
brainfuck ptr ms (pre, ']' : src)
	| ms !! ptr /= 0 = brainfuck ptr ms . tekcarb 1 $ (pre, ']' : src)
	| otherwise = brainfuck ptr ms (']' : pre, src)
brainfuck ptr ms (pre, _ : src) = brainfuck ptr ms (pre, src)

inc, dec :: Int -> [Int] -> [Int]
inc = modify succ
dec = modify pred

modify :: (Int -> Int) -> Int -> [Int] -> [Int]
modify f ptr ms | length ms < ptr + 1 =
	modify f ptr $ ms ++ replicate (ptr + 1 - length ms) 0
modify f ptr ms = take ptr ms ++ [f $ ms !! ptr] ++ drop (ptr + 1) ms

bracket, tekcarb :: Int -> (String, String) -> (String, String)
bracket n pp | n < 1 = pp
bracket n (pre, "") = error "no match bracket"
bracket n (pre, '[' : pst) = bracket (n + 1) ('[' : pre, pst)
bracket n (pre, ']' : pst) = bracket (n - 1) (']' : pre, pst)
bracket n (pre, c : pst) = bracket n (c : pre, pst)
tekcarb n pp | n < 1 = pp
tekcarb n ("", pst) = error "no match bracket"
tekcarb n (']' : pre, pst) = tekcarb (n + 1) (pre, ']' : pst)
tekcarb n ('[' : pre, pst) = tekcarb (n - 1) (pre, '[' : pst)
tekcarb n (c : pre, pst) = tekcarb n (pre, c : pst)
