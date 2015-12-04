import Control.Applicative ((<$>))
import Control.Arrow (first, (***))

import Primitive (env0)
import Eval (eval)
import Parse (parse, tokens)
import Environment (Env, Value, showValue)

main :: IO ()
main = interact $ maybe "err" (unlines . map showValue . fst) . (`scheme` env0)

scheme :: String -> Env -> Maybe ([Value], Env)
scheme src e = (`evaluate` e) =<< parse =<< tokens src

evaluate :: [Value] -> Env -> Maybe ([Value], Env)
evaluate [] e = Just ([], e)
evaluate (v : vs) e = uncurry (<$>) . (first . (:) *** evaluate vs) =<< eval v e
