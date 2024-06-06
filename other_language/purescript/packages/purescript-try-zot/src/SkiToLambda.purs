module SkiToLambda where

import Prelude

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

-- readSki :: Int -> String -> Tuple Lambda (Tuple Int String)
-- readSki

-- \n   `       s       k       i
-- 0x0a 0x60    0x73    0x6b    0x69
