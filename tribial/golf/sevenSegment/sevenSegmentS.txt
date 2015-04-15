import Data.List
main=interact$unlines.map concat.transpose.reverse.map(m.(i!!)).unfoldr u.read
o=[0,0,1,0,0,1]
p=[1,1,1,1,0]
u 0=Nothing
u n=Just(n`mod`10,n`div`10)
v=" |"
m[a,b,c,d,e,f,g]=[["   "," _ "]!!a,[v!!b," _"!!c,v!!d],[v!!e," _"!!f,v!!g]]
i=[[],0:o,1:0:p,[],[],[],[],1:o,replicate 7 1,p++[1,1]]
