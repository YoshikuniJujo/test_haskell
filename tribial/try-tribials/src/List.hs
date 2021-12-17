module List where

import Data.List

monosiri :: [String]
monosiri = [ 'モ' : nosiri ++ "ックカーネル" | nosiri <- permutations "ノリシ" ]
