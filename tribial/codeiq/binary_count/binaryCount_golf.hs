import Data.List
main=putStr$show(f$10^3)++" "++show(f$10^10)
f 0=0
f n|odd n=2*f(div n 2)+(n+1)`div`2|1>0=f(n-1)+sum(unfoldr(\n->case n of 0->Nothing;_->Just(n`mod`2,n`div`2))n)
