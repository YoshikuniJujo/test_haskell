f=1:1:zipWith(+)f(tail f)
main=interact$show.(f!!).read
