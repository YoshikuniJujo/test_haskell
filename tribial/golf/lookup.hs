import Data.List
main=interact$(\(x,y)->show x++","++show y).(\([c]:s)->u c s).lines
u a(l:s)=case a`elemIndices`l of x:_->(x,0);_->(\(x,y)->(x,y+1))$u a s
