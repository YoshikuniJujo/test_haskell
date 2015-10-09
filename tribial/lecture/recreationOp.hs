data Cap = Red | Blue | Yellow deriving Show

data Order = Cap :-> Cap deriving Show

recreation :: Cap -> [Order] -> Cap
recreation c [] = c
recreation Red (Red :-> c : os) = recreation c os
recreation Blue (Blue :-> c : os) = recreation c os
recreation Yellow (Yellow :-> c : os) = recreation c os
recreation c (_ : os) = recreation c os
