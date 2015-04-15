main=interact$concatMap(:"\n").s"".lines
s _[]=""
s(c:t)("pop":o)=c:s t o
s t(c:o)=s(c!!4:t)o
