import Data.List
main=interact$m.lines
m[a,b]|a`isPrefixOf`b=b
m[a:b,c]=a:m[b,c]
