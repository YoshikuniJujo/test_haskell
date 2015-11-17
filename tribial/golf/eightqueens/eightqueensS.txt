import Data.List
main=interact$unlines.map(>>=show).(\n->f n n[][][]).read
f _ 0 _ _ _=[[]]
f n i l h s=[q:r|q<-[0..n-1]\\(l++h++s),r<-f n(i-1)(fmap pred$q:l)(q:h)(fmap(+1)$q:s)]
