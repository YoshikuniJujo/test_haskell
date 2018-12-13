main=interact$unlines.map(uncurry(:).g.read).tail.lines
g n=(s!!(n-m),' ':s)where((m,_),s)=last.takeWhile((n>=).fst.fst).scanl(\((_,a),_)(b,c)->((a,a+b),c))((0,1),"")$map((\s->(length s,s)).show.(^2))[1..]
