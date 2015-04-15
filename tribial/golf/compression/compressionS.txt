import Data.List
main=interact$concatMap s.map(\x->(head x,length x)).group
s(c,1)=[c]
s(c,n)=c:show n
