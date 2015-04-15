f@(_:t)=1:1:zipWith(+)f t
main=interact$show.(f!!).read
