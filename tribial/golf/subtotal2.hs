import Data.List
main=interact$unlines.map(\(n,a)->n++" "++show a).map(\p->(fst$head p,sum$map snd p)).groupBy((.fst).(==).fst).sort.map(\l->let[n,a]=words l in(n,read a)).lines
