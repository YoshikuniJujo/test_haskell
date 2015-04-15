main=interact$show.(\x->x-5^5).r 3125.map((\(Just n)->n).flip lookup(zip"IVXLCDM"[1,5,10,50,100,500,1000]))
r p[]=p
r p(n:s)|p<n=r n s-p|0<1=p+r n s
