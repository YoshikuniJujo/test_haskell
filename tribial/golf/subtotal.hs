import Data.List
import Data.Function
main=interact$unlines.map(\(n,a)->n++" "++show a).s.map(\l->let[n,a]=words l in(n,read a)).lines
s=map(\p->(fst$head p,sum$map snd p)).groupBy(on(==)fst).sort
