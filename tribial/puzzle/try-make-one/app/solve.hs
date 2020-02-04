{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.List
import Data.Ratio

main :: IO ()
main = putStrLn `mapM_` (showExp <$> ret)

ret :: [Exp Integer]
ret = filter ((&&) <$> ((== 1) . calc) <*> asc) $ test <$> permutations [1 .. 9]

infixl 6 :+; infixl 7 :/

data Exp n = Num n | Digit2 n n | Exp n :/ Exp n | Exp n :+ Exp n deriving Show

showExp :: Show n => Exp n -> String
showExp (Num n) = show n
showExp (Digit2 m n) = show m ++ show n
showExp (l :/ r) = showExp l ++ " / " ++ showExp r
showExp (l :+ r) = "(" ++ showExp l ++ " + " ++ showExp r ++ ")"

calc :: Integral n => Exp n -> Ratio n
calc (Num n) = n % 1
calc (Digit2 m n) = (10 * m + n) % 1
calc (l :/ r) = calc l / calc r
calc (l :+ r) = calc l + calc r

test :: [n] -> Exp n
test [a, b, c, d, e, f, g, h, i] =
	Num a :/ Digit2 b c :+ Num d :/ Digit2 e f :+ Num g :/ Digit2 h i
test _ = error "Number of number should be 9"

asc :: Integral n => Exp n -> Bool
asc (l :+ r) = asc l && asc r && calc (lastTerm l) <= calc (headTerm r)
asc _ = True

headTerm, lastTerm :: Exp n -> Exp n
headTerm (l :+ _) = headTerm l; headTerm e = e
lastTerm (_ :+ r) = lastTerm r; lastTerm e = e
