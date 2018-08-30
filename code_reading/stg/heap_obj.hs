module HeapObj where

data Con = C Bool Int Char

some :: Con
some = C False 123 'c'
