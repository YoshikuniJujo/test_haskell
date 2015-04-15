main=interact$(\[s,t]->r s$read t).lines
d="0123456789"
r""_=""
r s t|all(`elem`d)s=show$read s*(floor t)|all(`elem`'.':d)s=show$read s*t|0<1=concat$replicate(floor t)s
