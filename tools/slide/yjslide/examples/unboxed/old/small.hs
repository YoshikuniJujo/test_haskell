{-# LANGUAGE MagicHash #-}

import GHC.Prim
import GHC.Types
import SignumDouble

main :: IO ()
main = do
	print $ 4 * (D# (1.0## +## (negateDouble# (1.0## /## 3.0##))))
	print $ 4 * (1.0 + negate (1.0 / 3.0))
	print $ 4 * (D# (getPi4' 0.0## 1.0## 2##))
	print $ D# (getI 1##)
	print $ 4 * (D# (getPi4'2 ()))

getPi4' :: Double# -> Double# -> Word# -> Double#
getPi4' p _ 0## = p
getPi4' p i n = getPi4' (p +## (1.0## /## i))
	(negateDouble# (i +## (signumDouble# i *## 2.0##))) (n `minusWord#` 1##)

getI :: Word# -> Double#
getI 0## = 1.0##
getI n = let i = getI (n `minusWord#` 1##) in
	negateDouble# (i +## (signumDouble# i *## 2.0##))

getPi4'2 :: () -> Double#
getPi4'2 () = 0.0## +## (1.0## /## getI 0##) +## (1.0## /## getI 1##)
