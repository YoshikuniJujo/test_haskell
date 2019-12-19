module ListConcatenation where

import Prelude hiding ((++))

(++) :: [a] -> [a] -> [a]
[] ++ r = r
(h : t) ++ r = h : (t ++ r)
