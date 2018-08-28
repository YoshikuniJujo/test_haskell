{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

import Ackermann

main :: IO ()
main = print $(litE . integerL $ ackermann 3 12)
