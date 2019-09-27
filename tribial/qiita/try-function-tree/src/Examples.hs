{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Examples where

import FunctionTree

f = ((const 123 . undefined) . undefined) . undefined
