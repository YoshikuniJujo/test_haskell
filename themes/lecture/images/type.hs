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
	putType t "black" TypeA 4

	goto t 60 5
	putType t "black" TypeB 4

	goto t 100 5
	putType t "black" TypeC 4

	hideturtle t
	svg <- getSVG t
	writeFile "type.svg" $ showSVG 150 45 svg
	waitField f

data Type = TypeA | TypeB | TypeC deriving Show

putType :: ColorClass c => Turtle -> c -> Type -> Double -> IO ()
putType t c TypeA s = do
	(x, y) <- position t
	pencolor t c
	penup t
	setheading t 0
	beginfill t
	replicateM_ 4 $ forward t (8 * s) >> right t 90
	endfill t
	goto t (x + 3.8 * s) (y + s)
	pencolor t "white"
	beginfill t
	replicateM_ 36 $ forward t (pi / 6 * s) >> right t 10
	endfill t
putType t c TypeB s = do
	(x, y) <- position t
	pencolor t c
	penup t
	setheading t 0
	beginfill t
	replicateM_ 4 $ forward t (8 * s) >> right t 90
	endfill t
	goto t (x + 4 * s) (y + s)
	setheading t (- 60)
	pencolor t "white"
	beginfill t
	replicateM_ 3 $ forward t (6 * s) >> right t 120
	endfill t
putType t c TypeC s = do
	(x, y) <- position t
	pencolor t c
	penup t
	setheading t 0
	beginfill t
	replicateM_ 4 $ forward t (8 * s) >> right t 90
	endfill t
	goto t (x + 1.6 * s) (y + 1.6 * s)
	setheading t 0
	pencolor t "white"
	beginfill t
	replicateM_ 4 $ forward t (5 * s) >> right t 90
	endfill t
