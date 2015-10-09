data Cap = Red | Blue | Yellow deriving Show

data Order = Change Cap Cap deriving Show

recreation :: Cap -> [Order] -> Cap
recreation c [] = c
recreation Red (Change Red c : os) = recreation c os
recreation Blue (Change Blue c : os) = recreation c os
recreation Yellow (Change Yellow c : os) = recreation c os
recreation c (_ : os) = recreation c os
