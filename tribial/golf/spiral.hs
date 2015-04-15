main=interact$unlines.map(init.concatMap((++"\t").show)).s.read
s 0=[]
s n=[1..n]:zipWith(\i->(++[i]))[n+1..n*2-1](reverse$map(map(+(2*n-1)).reverse).s$n-1)
