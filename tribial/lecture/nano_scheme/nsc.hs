{-# LANGUAGE TupleSections #-}

import Primitive
import Parse
import Environment
import Maybe

main :: IO ()
main = interact $ maybe "err" (unlines . map showValue . fst) . (`scheme` env0)

scheme :: String -> Env -> Maybe ([Value], Env)
scheme src e = (, e) `mapply` (parse `mbind` tokens src)
