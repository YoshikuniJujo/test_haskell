{-# LANGUAGE PackageImports #-}

import Control.Applicative
import Control.Arrow
import "monads-tf" Control.Monad.State
import Data.List
import Data.Ratio
import System.IO.Unsafe

main = interact $ show . numerator . c . a . map e . lines

data R = R (Ratio Int) | R :> R | R :| R | NoR

c (R r) = r
c (r1 :> r2) = c r1 + c r2
c (r1 :| r2) = c r1 * c r2 / (c r1 + c r2)
c NoR = 0

data W = St | Ed | UpSt | DnSt | Up | Dn | Br | Jn | Rs Int deriving Eq

e "" = []
e ('@' : '-' : s) = St : e s
e ('-' : '@' : s) = Ed : e s
e ('/' : '-' : s) = UpSt : e s
e ('\\': '-' : s) = DnSt : e s
e ('/' : s) = Up : e s
e ('\\': s) = Dn : e s
e ('<' : s) = Br : e s
e ('>' : s) = Jn : e s
e ('|' : s) =
	let (n, '|' : r) = span (/= '|') s in Rs (read n) : e r
e ('-' : s) = e s
e (' ' : s) = e s

a wss
	| Just i <- findIndex (elem St) wss = fst $ f Nothing `runState` (i, wss)
	| otherwise = error "bad"

f b = do
	w <- popWire
	case w of
		St -> f b
		UpSt -> f b
		DnSt -> f b
		Ed -> if b == Nothing then return NoR else error "bad"
		Dn -> do
			downWire
			i <- gets fst
			case b of
				Just bs -> if i == bs
					then return NoR else f b
				_ -> error "bad"
		Up -> do
			upWire
			i <- gets fst
			case b of
				Just bs -> if i == bs
					then return NoR else f b
				_ -> error "bad"
		Br -> do
			i <- gets fst
			u <- upWire >> f (Just i)
			d <- downWire >> f (Just i)
			Jn <- popWire
			n <- f b
			return $ u :| d :> n
		Rs r -> (R (r % 1) :>) <$> f b

popWire = do
	(i, wss) <- get
	let (w, wss') = popW i wss
	put (i, wss')
	return w
upWire = modify (first pred)
downWire = modify (first succ)

popW 0 ((w : ws) : wss) = (w, ws : wss)
popW n (ws : wss) = (ws :) `second` popW (n - 1) wss
