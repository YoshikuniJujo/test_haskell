import Data.List
main=interact$unlines.(\(w:s)->b w.t w$map(r w.l w)s).lines
x=transpose
v=reverse
l _""=""
l w a@(c:s)|w`isPrefixOf`a=map(const ' ')w++drop(length w)a|0<1=c:l w s
r w=v.l w.v
t w=x.map(l w).x
b w=x.map(r w).x
