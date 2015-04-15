main=interact$concatMap(:"\n").s"".lines
s _[]=""
s t(['a','d','d',' ',c]:o)=s(c:t)o
s(c:t)("pop":o)=c:s t o
