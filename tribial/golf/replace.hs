main=interact$ \s->r(head s)(last s)s
r _ _""=""
r c d(x:s)|x==c=d:r c d s|1>0=x:r c d s
