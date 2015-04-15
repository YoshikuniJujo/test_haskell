main=interact$unwords.w.read
(#)=div
(%)=mod
s=["","one","two","three","four","","six","seven","","nine"]
t=["","","twenty","","","fifty","","","eighty"]
w n|n<10=[s!!n]|n<20=["thirteen"]|n<90=t!!(n#10):w(n%10)|n<800=w(n#100)++"hundred":w(n%100)|n<10^6=w(n#1000)++"thousand":w(n%1000)|0<1=w(n#(10^6))++"million":w(n%(10^6))
