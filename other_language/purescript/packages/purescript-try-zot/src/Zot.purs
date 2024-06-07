-- module Zot (main, mainFile) where
module Zot where

import Control.Monad
import Data.Semigroup
import Data.Eq
import Data.Enum
import Data.Show
import Data.Function
import Data.Maybe
import Data.List (List(..), (:), foldM, fromFoldable, concatMap)
import Data.String
import Effect
import Data.Unit
import Effect.Console

data Fun = Fun (Fun -> Effect Fun) | Zero | One

mkFun :: (Fun -> Fun) -> Fun
mkFun f = Fun $ pure <<< f

instance Show Fun where
        show Zero = "0"
        show One = "1"
        show (Fun _) = "function"

stringToIntList :: String -> List Int
stringToIntList = map fromEnum <<< fromFoldable <<< toCodePointArray

codePointToFun :: Int -> Maybe Fun
codePointToFun 48 = Just Zero
codePointToFun 49 = Just One
codePointToFun _ = Nothing

dd :: Fun -> Fun -> Effect Fun
dd (Fun f) = f
dd Zero = dd $ zero unit
dd One = dd one

infixl 9 dd as $$

gtdd :: Effect Fun -> Fun -> Effect Fun
gtdd f a = f >>= (_ $$ a)

infixl 6 gtdd as >$$

ddlt :: Fun -> Effect Fun -> Effect Fun
ddlt f a = (f $$ _) =<< a

infixr 8 ddlt as $$<

gtddlt :: Effect Fun -> Effect Fun -> Effect Fun
gtddlt f a = f >>= (_ $$< a)

infixl 7 gtddlt as >$$<

cont :: Fun -> Fun
cont f = Fun (_ $$ f)

s :: Fun
s = mkFun $ \x -> mkFun $ \y -> Fun $ \z -> x $$ z >$$< y $$ z

k :: Fun
k = mkFun $ mkFun <<< const

i :: Fun
i = mkFun id

id :: forall a . a -> a
id x = x

empty :: Fun
empty = cont i

zero :: Unit -> Fun
zero _ = cont (Fun (\f -> f $$ s >$$ k))

one :: Fun
one = mkFun $ \c -> cont $ mkFun $ \l -> cont $ Fun $ \r -> c $$< l $$ r

pr :: (String -> Effect Unit) -> Fun
pr op = Fun $ \x -> (((interrogate x >$$ Zero >$$ One) >>= (op <<< show)) *> pure (pr op))

interrogate :: Fun -> Effect Fun
interrogate f = f $$ i >$$ i >$$ i >$$ k

output :: Effect Fun
output = k $$< k $$< k $$< k $$< k $$< k $$ i

readZot :: String -> Effect Fun
readZot = foldM ($$) empty <<< readListFun

readListFun :: String -> List Fun
readListFun = mapMaybe codePointToFun <<< stringToIntList

mapMaybe :: forall a b . (a -> Maybe b) -> List a -> List b
mapMaybe _f Nil = Nil
mapMaybe f (x : xs) = case f x of
        Nothing -> mapMaybe f xs
        Just y -> y : mapMaybe f xs

interpret :: (String -> Effect Unit) -> String -> Effect Unit
interpret op src = (readZot (removeComment src) >$$< output >$$ pr op) *> pure unit
        where
        removeComment = joinWith "" <<< map (takeWhile (_ /= codePointFromChar '#')) <<< split (Pattern "\n")

hello =
        "11110101010011101010100100110101001001001110101010011101010100110101001010101001" <>
        "11010101001101010011010101001101010010101010011101010100111010101001101010010101" <>
        "01001110101010011010100110101010011010100101010100111010101001101010011010101001" <>
        "10101001101010100111010101001110101010011101010100111010101001001101010010011010" <>
        "10010011010100100110101001010100111010101001101010011010101001101010011010101001" <>
        "10101001010100111010101001110101010011010100101010100111010101001101010011010101" <>
        "00110101001010101001110101010011010100110101010011101010100110101001010101001010" <>
        "10011101010100110101001010100111010101001110101010011010100101010100111010101001" <>
        "10101001010100111010101001101010010101010010101001101010011101010100110101001101" <>
        "01010010010101001101010010101001110101010011010100110101010011010100110101010011" <>
        "01010010101001110101010011101010100110101001010101001110101010011010100110101010" <>
        "01101010010101010011101010100110101001101010100111010101001101010010101010010101" <>
        "00111010101001101010010101001110101010011101010100110101001010101001110101010011" <>
        "01010010101001110101010011010100101010100101010011010100111010101001101010011010" <>
        "101001001010100110101001010100111010101001101010010101001010100" <>
        "01010000100001000010011000110110010011101111011011101110000001000011010011110110" <>
        "00110110001101101010011000010010"

rev =
        "1111010101001110101010010011010100100100" <>
        "1110101010011101010100110101001010101001" <>
        "1101010100110101001101010100110101001010" <>
        "1010011101010100111010101001101010010101" <>
        "0100111010101001101010011010101001101010" <>
        "0101010100111010101001101010011010101001" <>
        "1010100110101010011101010100111010101001" <>
        "1101010100111010101001001101010010011010" <>
        "1001001101010010011010100101010011101010" <>
        "1001101010011010101001101010011010101001" <>
        "1010100101010011101010100111010101001101" <>
        "0100101010100111010101001101010011010101" <>
        "0011010100101010100111010101001101010011" <>
        "0101010011101010100110101001010101001010" <>
        "1001110101010011010100101010011101010100" <>
        "1110101010011010100101010100111010101001" <>
        "1010100101010011101010100110101001010101" <>
        "0010101001101010011101010100110101001101" <>
        "0101001001010100110101001010100111010101" <>
        "0011010100110101010011010100110101010011" <>
        "0101001010100111010101001110101010011010" <>
        "1001010101001110101010011010100110101010" <>
        "0110101001010101001110101010011010100110" <>
        "1010100111010101001101010010101010010101" <>
        "0011101010100110101001010100111010101001" <>
        "1101010100110101001010101001110101010011" <>
        "0101001010100111010101001101010010101010" <>
        "0101010011010100111010101001101010011010" <>
        "1010010010101001101010010101001110101010" <>
        "01101010010101001010100" <> "1110001010001"
