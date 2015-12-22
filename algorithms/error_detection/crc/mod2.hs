import Control.Arrow
import Data.List
import Data.Bits

mod2 :: Bits a => a -> a -> a
mod2 x y = rev' $ md2 (rev x) (rev1 y)

md2 :: Bits a => (a, Int) -> (a, Int) -> (a, Int)
md2 (x, m) (_, n) | m < n = (x, m)
md2 (x, m) yn@(y, _)
	| x `testBit` 0 = md2 (x `shiftR` 1 `xor` y, m - 1) yn
	| otherwise = md2 (x `shiftR` 1, m - 1) yn

rev' :: Bits a => (a, Int) -> a
rev' (x, n) = fromBinary . pad False n . reverse $ binary x

rev :: Bits a => a -> (a, Int)
rev = (fromBinary . reverse &&& length) . binary

rev1 :: Bits a => a -> (a, Int)
rev1 = (fromBinary . tail . reverse &&& length) . binary

fromBinary :: Bits a => [Bool] -> a
fromBinary (b : bs) =
	(if b then bit 0 else zeroBits) .|. (fromBinary bs `shiftL` 1)
fromBinary _ = zeroBits

binary :: Bits a => a -> [Bool]
binary = unfoldr popBit

popBit :: Bits a => a -> Maybe (Bool, a)
popBit x | 0 <- popCount x = Nothing
popBit x = Just (x `testBit` 0, x `shiftR` 1)

bins :: Bits a => String -> a
bins = fromBinary . reverse . map (== '1')

pad :: a -> Int -> [a] -> [a]
pad z n xs = replicate (n - length xs) z ++ xs
