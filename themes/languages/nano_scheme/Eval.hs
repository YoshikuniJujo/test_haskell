{-# LANGUAGE TupleSections #-}

module Eval (eval) where

import Control.Applicative ((<$>))
import Environment (Env, refer)
import Value (Value(..), showValue, Error(..), ErrorMessage)

appErr :: ErrorMessage
appErr = "*** ERROR: invalid application: "

eval :: Value -> Env -> Either Error (Value, Env)
eval (Symbol s) e = (, e) <$> refer s e
eval (Cons v vs) e = uncurry (flip apply vs) =<< eval v e
eval v e = Right (v, e)

apply :: Value -> Value -> Env -> Either Error (Value, Env)
apply DoExit Nil _ = Left Exit
apply f as _ = Left . Error $ appErr ++ showValue (f `Cons` as)
