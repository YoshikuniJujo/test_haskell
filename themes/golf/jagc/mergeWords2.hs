main=interact$m.lines
m[a,b]|a!b=b
m[a:b,c]=a:m[b,c]
""!_=1>0
(a:b)!(x:y)|a==x=b!y|1>0=0>1
