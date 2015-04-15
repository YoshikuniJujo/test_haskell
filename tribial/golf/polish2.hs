main=interact$show.p[].reverse.words
p(x:_)[]=x
p(x:y:s)([c]:o)|elem c"+-*"=p(t c x y:s)o
p s(d:o)=p(read d:s)o
t '+'=(+)
t '-'=(-)
t '*'=(*)
