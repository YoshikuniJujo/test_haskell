import Numeric
import Data.List
m=map
a=reverse
main=interact$unlines.m(a.dropWhile(==' ').a.intercalate"  ").s.m((\[r,n]->(read r,n)).words).lines
s e=("\\  ":m((++" ").snd)e):zipWith(:)(m snd e)(m(m i.((\x->m((/x).fst)e).fst))e)
i 1="1   "
i x=showFFloat(Just 2)x""
