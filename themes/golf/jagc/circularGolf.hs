main=interact$unlines.map(w.i.words).tail.lines
w b|b="true"|0<1="false"
c s@(h:t)=s:c(t++[h])
i[s,t]=elem s$take(length t)$c t
