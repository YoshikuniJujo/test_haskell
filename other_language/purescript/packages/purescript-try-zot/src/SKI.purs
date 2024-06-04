module SKI where

import Data.Semigroup
import Data.Functor
import Data.Function
import Data.Eq
import Data.Enum
import Data.Show
import Data.Tuple
import Data.Maybe
import Data.List
import Data.Boolean
import Data.String
import Effect.Exception
import Effect.Unsafe

import Data.Show.Generic
import Data.Generic.Rep

data Rec a
        = In (Rec a -> Rec a)
        | Int Int | Char Char | Bool Boolean | Error String

instance Eq (Rec a) where
        eq (Int i) (Int j) = i == j
        eq (Char c) (Char d) = c == d
        eq (Bool b) (Bool c) = b == c
        eq _ _ = false

instance Show (Rec a) where
        show (In _ ) = "function"
        show (Int i) = show i
        show (Char c) = show c
        show (Bool b) = show b
        show (Error e) = "Error: " <> e

suc :: forall a . Rec a
suc = In scc
        where
        scc (Int i) = case succ i of
                Nothing -> Error "reached the upper limit"
                Just i' -> Int i'
        scc (Char c) = case succ c of
                Nothing -> Error "reached the upper limit"
                Just c' -> Char c'
        scc _ = Error "not int"

out :: forall a . Rec a -> Rec a -> Rec a
out x = case x of
        In f -> f
        _ -> \_ -> Error "not function"

ii :: forall a . Rec a
ii = In \x -> x

k :: forall a . Rec a
k = In \x -> In \_ -> x

s :: forall a . Rec a
s = In \x -> In $ \y -> In $ \z -> (out $ out x z) (out y z)

readChurch :: forall a . Rec a -> Rec a
readChurch cn = cn `out` suc `out` Int 0

data SKI = Gr | S | K | I

derive instance Generic SKI _

instance Show SKI where show = genericShow

mkski :: forall a . String -> Rec a
mkski = fst <<< makeSKI <<< mapMaybe codePointToSki <<< stringToIntList

makeSKI :: forall a . List SKI -> Tuple (Rec a) (List SKI)
makeSKI Nil = Tuple (Error "no input") Nil
makeSKI (Gr : rest) = let
        (Tuple c rest') = makeSKI rest
        (Tuple c' rest'') = makeSKI rest' in case c of
                In f -> Tuple (f c') rest''
                _ -> Tuple (Error "in makeSKI") rest''
makeSKI (S : rest) = Tuple s rest
makeSKI (K : rest) = Tuple k rest
makeSKI (I : rest) = Tuple ii rest

stringToIntList :: String -> List Int
stringToIntList = map fromEnum <<< fromFoldable <<< toCodePointArray

codePointToSki :: Int -> Maybe SKI
codePointToSki 0x60 = Just Gr
codePointToSki 0x60 = Just Gr
codePointToSki 0x73 = Just S
codePointToSki 0x6b = Just K
codePointToSki 0x69 = Just I
codePointToSki _ = Nothing

mapMaybe :: forall a b . (a -> Maybe b) -> List a -> List b
mapMaybe _f Nil = Nil
mapMaybe f (x : xs) = case f x of
        Nothing -> mapMaybe f xs
        Just y -> y : mapMaybe f xs

sc :: forall a . Rec a
sc = mkski "`s``s`ks``s`kki"

mul :: forall a . Rec a -> Rec a -> Rec a
mul x y = out (out x (out y sc)) (out k ii)

zero :: forall a . Rec a
zero = mkski "`ki"
one = mkski "i"
two = mkski "``s``s`kski"
three = mkski "``s``s`ksk``s``s`kski"
