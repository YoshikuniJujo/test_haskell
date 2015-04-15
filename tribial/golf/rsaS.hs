main=interact$show.(\[p,q,e]->let n=(p-1)*(q-1);c=g 1 0 n e in c+(1-signum c)*n`div`2).map read.lines
g _ d _ 0=d
g b d m n=g(d-m`div`n*b)b n$m`mod`n
