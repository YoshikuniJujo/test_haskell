main=interact$show.p[].reverse.words
p(x:_)[]=x
p(x:y:s)("+":o)=p(x+y:s)o
p(x:y:s)("-":o)=p(x-y:s)o
p(x:y:s)("*":o)=p(x*y:s)o
p s(d:o)=p(read d:s)o
