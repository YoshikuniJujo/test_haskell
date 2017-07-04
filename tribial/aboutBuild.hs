import GHC.Base hiding (foldr)

myConcat :: Foldable t => t [a] -> [a]
myConcat xs = foldr (\x y -> foldr (:) y x) [] xs

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = build (\c n -> foldr (c . f) n xs)

nums :: [Integer]
nums = myMap (* 3) $ myMap (+ 5) [3, 8, 2]

{-
myMap (+ 5) [3, 8, 2]
-> build $ \c1 n1 -> foldr (c1 . (+ 5)) n1 [3, 8, 2]

myMap (* 3) $ myMap (+ 5) [3, 8, 2]
-> build $ \c2 n2 -> foldr (c2 . (* 3)) n2 $ myMap (+ 5) [3, 8, 2]
-> build $ \c2 n2 -> foldr (c2 . (* 3)) n2
	$ build $ \c1 n1 -> foldr (c1 . (+ 5)) n1 [3, 8, 2]
		|
	(foldr c n (build g) ==> g c n)
		|
		V
-> build $ \c2 n2 -> (\c1 n1 -> foldr (c1 . (+ 5)) n1 [3, 8, 2]) (c2 . (* 3)) n2
-> build $ \c2 n2 -> foldr ((c2 . (* 3)) . (+ 5)) n2 [3, 8, 2]
-> foldr ((:) . (* 3) . (+ 5)) [] [3, 8, 2]
-}
