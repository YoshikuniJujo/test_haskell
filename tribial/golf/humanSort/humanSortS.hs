import Data.List
(o#f)x y=f x`o`f y
main=interact$unlines.sortBy(compare#h).lines
h s=maybe(read s)(read(init s)*).lookup(last s).zip"bKMGTP"$iterate(*1024)1
