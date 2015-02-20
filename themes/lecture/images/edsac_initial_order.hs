import Control.Monad
import System.Environment
import Text.XML.YJSVG hiding (topleft)
import Graphics.X11.Turtle

main :: IO ()
main = do
	fn : _ <- getArgs
	src <- readFile fn
	let	bs = groupN 34 . readBits $ preprocess src
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	hideturtle t
	bgcolor t "black"
	pencolor t "green"
	pensize t 3
	penup t
	goto t 20 10
	showBitsLines t bs -- initialOrder
	hideturtle t
	svg <- getSVG t
	writeFile "edsac_initial_order.svg" $ showSVG 375 255 svg
	waitField f

data Bit = O | I deriving Show

zero :: Turtle -> IO ()
zero t = do
	pendown t
	replicateM_ 2 $ forward t 5 >> right t 90 >> forward t 10 >> right t 90
	penup t
	setheading t 0
	forward t 10

one :: Turtle -> IO ()
one t = do
	(x, y) <- position t
	forward t 5
	setheading t (- 90)
	pendown t
	forward t 10
	penup t
	goto t x y
	setheading t 0
	forward t 10

initialOrder :: [[Bit]]
initialOrder = [
	[
		O, O, I, O, I,  O,  O, O, O, O, O, O, O, O, O, O,  O,	-- 1
		I, O, I, O, I,  O,  O, O, O, O, O, O, O, O, I, O,  O ],	-- 2
	[	O, O, I, O, I,  O,  O, O, O, O, O, O, O, O, O, O,  O,	-- 3
		O, O, O, I, I,  O,  O, O, O, O, O, O, O, I, I, O,  O ],	-- 4
	[	O, O, O, O, O,  O,  O, O, O, O, O, O, O, O, O, I,  O,	-- 5
		O, O, O, O, O,  O,  O, O, O, O, O, O, O, I, O, I,  O ],	-- 6
	[	O, O, I, O, I,  O,  O, O, O, O, O, O, O, O, O, O,  O,	-- 7
		O, I, O, O, O,  O,  O, O, O, O, O, O, O, O, O, O,  O ],	-- 8
	[	I, I, I, O, O,  O,  O, O, O, O, O, O, O, O, O, O,  O,	-- 9
		O, O, I, O, O,  O,  O, O, O, O, O, I, O, O, O, O,  O ],	-- 10
	[	O, O, I, O, I,  O,  O, O, O, O, O, O, O, O, O, O,  I,	-- 11
		O, I, O, O, O,  O,  O, O, O, O, O, O, O, O, I, O,  O ],	-- 12
	[	I, I, I, O, O,  O,  O, O, O, O, O, O, O, O, I, O,  O,	-- 13
		O, I, I, O, O,  O,  O, O, O, O, O, O, O, I, O, I,  O ],	-- 14
	[	O, O, O, I, I,  O,  O, O, O, O, O, I, O, I, O, I,  O,	-- 15
		O, O, I, O, I,  O,  O, O, O, O, O, O, O, O, I, I,  O ],	-- 16
	[	I, I, I, I, I,  O,  O, O, O, O, O, O, O, O, O, I,  O,	-- 17
		I, I, O, O, I,  O,  O, O, O, O, O, O, I, O, O, O,  O ],	-- 18
	[	I, I, I, O, O,  O,  O, O, O, O, O, O, O, O, I, O,  O,	-- 19
		O, O, I, O, I,  O,  O, O, O, O, O, O, O, O, O, I,  O ],	-- 20
	[	O, O, O, I, I,  O,  O, O, O, O, O, O, I, O, I, I,  O,	-- 21
		O, O, I, O, O,  O,  O, O, O, O, O, O, O, I, O, O,  O ]	-- 22
--	[	I, I, I, O, O,  O,  O, O, O, O, O, O, O, O, O, I,  O,	-- 23
	]

showBitsLines :: Turtle -> [[Bit]] -> IO ()
showBitsLines t [] = return ()
showBitsLines t (bs : bss) = do
	showBits t bs
	penup t
	setheading t (- 90)
	forward t 15
	setx t 20
	setheading t 0
	showBitsLines t bss

showBits :: Turtle -> [Bit] -> IO ()
showBits t [] = return ()
showBits t (O : bs) = zero t >> showBits t bs
showBits t (I : bs) = one t >> showBits t bs

preprocess :: String -> String
preprocess = unlines . map (takeWhile (/= '\t')) . lines

readBits :: String -> [Bit]
readBits "" = []
readBits ('0' : bs) = O : readBits bs
readBits ('1' : bs) = I : readBits bs
readBits (' ' : bs) = readBits bs
readBits ('\n' : bs) = readBits bs
readBits _ = error "bad"

groupN :: Int -> [a] -> [[a]]
groupN n [] = []
groupN n xs = take n xs : groupN n (drop n xs)
