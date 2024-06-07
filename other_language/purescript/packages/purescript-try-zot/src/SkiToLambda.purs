module SkiToLambda where

import Prelude
import Data.Foldable
import Data.Enum
import Data.Tuple
import Data.Maybe
import Data.List
import Data.String hiding (take)

data Lambda = Var String | Apply Lambda Lambda | Fun String Lambda | K | I

infixl 9 Apply as :$:
infixr 8 Fun as :->

instance Show Lambda where
        show (Var v) = v
        show ap@(_ :$: _) = showAp ap
                where
                showAp (f :$: a) = showAp f <> " " <> par show a
                showAp e = par show e
                par sh a@(Apply _ _) = "(" <> sh a <> ")"
                par sh f@(Fun _ _) = "(" <> sh f <> ")"
                par sh e = sh e
        show f@(Fun _ _) = "\\" <> showFun f
                where
                showFun (Fun p e) = p <> " " <> showFun e
                showFun e = "-> " <> show e
        show K = "K"
        show I = "I"

size :: Lambda -> Int
size (f :$: a) = size f + size a
size (_ :-> e) = 1 + size e
size _ = 1

skiToLabmda :: Partial => String -> Lambda
skiToLabmda s = case minimumBy (comparing size) <<< iterate 15 beta $ readSki s of
                Just lmbd -> betaKI lmbd

readSki :: Partial => String -> Lambda
readSki s = case readSkiGen 0
        <<< map fromEnum <<< fromFoldable $ toCodePointArray s of
        Tuple r (Tuple _ Nil) -> r

readSkiGen :: Partial => Int -> List Int -> Tuple Lambda (Tuple Int (List Int))
readSkiGen n (0x60 : cs) = let
        Tuple f ncs = readSkiGen n cs
        Tuple a ncs' = uncurry readSkiGen ncs in
        Tuple (f :$: a) ncs'
readSkiGen n (c : cs) = (\l -> Tuple l (Tuple (n + 1) cs)) case c of
        0x69 -> fx x
        0x6b -> fx $ fy x
        0x73 -> fx $ fy $ fz $ x :$: z :$: (y :$: z)
        where
        nmx = "x" <> show n
        fx = (nmx :-> _)
        x = Var nmx
        nmy = "y" <> show n
        fy = (nmy :-> _)
        y = Var nmy
        nmz = "z" <> show n
        fz = (nmz :-> _)
        z = Var nmz

-- \n   `       s       k       i
-- 0x0a 0x60    0x73    0x6b    0x69

beta :: Lambda -> Lambda
beta (fun :$: arg) = case beta fun of
        p :-> e -> para p (beta arg) e
        ex -> ex :$: beta arg
        where
        para p a v@(Var x)
                | p == x = a
                | otherwise = v
        para p a (f :$: b) = para p a f :$: para p a b
        para p a f@(q :-> e)
                | p == q = f
                | otherwise = q :-> para p a e
        para _ _ ki = ki
beta (p :-> e) = p :-> beta e
beta kiv = kiv

betaKI :: Lambda -> Lambda
betaKI ((p :-> v@(Var x)) :$: ar)
        | p == x = ar
        | otherwise = v
betaKI ((p :-> f@(q :-> Var x)) :$: a)
        | q == x = f
        | p == x = "_" :-> a
betaKI (f :$: a) = betaKI f :$: betaKI a
betaKI (p :-> e) = p :-> betaKI e
betaKI kiv = kiv

iterate :: forall a . Int -> (a -> a) -> a -> List a
iterate n _ _ | n < 1 = Nil
iterate n f x = x : iterate (n - 1) f (f x)
