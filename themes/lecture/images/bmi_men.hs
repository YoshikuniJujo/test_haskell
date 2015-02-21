import Control.Monad
import Text.XML.YJSVG hiding (topleft)
import Graphics.X11.Turtle

main :: IO ()
main = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	penup t

	goto t 20 5
	putMan t "black" Yase 4
	goto t 30 15
	putDigit t (0x00, 0xff, 0x00) one 2
	goto t 42 15
	putDigit t (0x00, 0xff, 0x00) six 2

	goto t 90 5
	putMan t "black" Futsu 4
	goto t 100 15
	putDigit t (0x00, 0xff, 0x00) two 2
	goto t 112 15
	putDigit t (0x00, 0xff, 0x00) three 2

	goto t 170 5
	putMan t "black" Pocha 4
	goto t 180 15
	putDigit t (0x00, 0xff, 0x00) three 2
	goto t 192 15
	putDigit t (0x00, 0xff, 0x00) zero 2

	hideturtle t
	svg <- getSVG t
	writeFile "bmi_men.svg" $ showSVG 245 135 svg
	waitField f

data Taikei = Yase | Futsu | Pocha deriving Show

putMan :: ColorClass c => Turtle -> c -> Taikei -> Double -> IO ()
putMan t c Yase s = do
	(x, y) <- position t
	penup t
	pencolor t c
	setheading t 0
	goto t (x + 4.5 * s) y
	beginfill t
	replicateM_ 36 $ forward t (pi / 4 * s) >> right t 10
	endfill t
	goto t (x + 0 * s) (y + 10 * s)
	beginfill t
	forward t $ 10 * s
	setheading t (- 120)
	replicateM_ 7 $ forward t (pi * s) >> left t 10
	setheading t 0
	backward t $ 10 * s
	setheading t 60
	replicateM_ 7 $ forward t (pi * s) >> left t 10
	endfill t
putMan t c Futsu s = do
	(x, y) <- position t
	penup t
	pencolor t c
	setheading t 0
	goto t (x + 4.5 * s) y
	beginfill t
	replicateM_ 36 $ forward t (pi / 4 * s) >> right t 10
	endfill t
	goto t (x + 0 * s) (y + 10 * s)
	beginfill t
	forward t $ 10 * s
	setheading t (- 90)
	forward t $ 21 * s
	setheading t 0
	backward t $ 10 * s
	setheading t 90
	forward t $ 21 * s
	endfill t
putMan t c Pocha s = do
	(x, y) <- position t
	penup t
	pencolor t c
	setheading t 0
	goto t (x + 4.5 * s) y
	beginfill t
	replicateM_ 36 $ forward t (pi / 4 * s) >> right t 10
	endfill t
	goto t (x + 0 * s) (y + 10 * s)
	beginfill t
	forward t $ 10 * s
	setheading t (- 60)
	replicateM_ 7 $ forward t (pi * s) >> right t 10
	setheading t 0
	backward t $ 10 * s
	setheading t 120
	replicateM_ 7 $ forward t (pi * s) >> right t 10
	endfill t

putDigit :: ColorClass c => Turtle -> c -> [Bool] -> Double -> IO ()
putDigit t c bs s = do
	(x, y) <- position t
	penup t
	pencolor t c
	pensize t s

	sequence_ . map snd . filter fst . zip bs $ bars x y

	where
	bars x y = [
		do
		setheading t 0
		goto t (x + s) y
		setheading t 0
		pendown t
		forward t $ 2 * s
		penup t,

		do
		setheading t 0
		goto t (x + s) (y + 4 * s)
		pendown t
		forward t $ 2 * s
		penup t,

		do
		setheading t 0
		goto t (x + s) (y + 8 * s)
		pendown t
		forward t $ 2 * s
		penup t,

		do
		setheading t (- 90)
		goto t (x + 0 * s) (y + 1 * s)
		pendown t
		forward t $ 2 * s
		penup t,

		do
		setheading t (- 90)
		goto t (x + 4 * s) (y + 1 * s)
		pendown t
		forward t $ 2 * s
		penup t,

		do
		setheading t (- 90)
		goto t (x + 0 * s) (y + 5 * s)
		pendown t
		forward t $ 2 * s
		penup t,

		do
		setheading t (- 90)
		goto t (x + 4 * s) (y + 5 * s)
		pendown t
		forward t $ 2 * s
		penup t ]

zero, one, two, three, four, five, six, seven, eight, nine :: [Bool]
zero = [True, False] ++ replicate 5 True
one = replicate 4 False ++ [True, False, True]
two = replicate 3 True ++ [False, True, True, False]
three = replicate 3 True ++ concat (replicate 2 [False, True])
four = [False, True, False, True, True, False, True] 
five = replicate 4 True ++ [False, False, True]
six = replicate 4 True ++ [False, True, True]
seven = True : replicate 3 False ++ [True, False, True]
eight = replicate 7 True
nine = replicate 5 True ++ [False, True]
