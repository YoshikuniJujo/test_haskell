import IOMcn

outArg :: IOMcn a b -> a -> IOMcn () b
outArg m x = arr (const x) >>> m

inArg :: (a -> IOMcn () b) -> IOMcn a b
inArg f = arr (\x -> (f x, ())) >>> app

type IO' = IOMcn ()

(>=>) :: (a -> IO' b) -> (b -> IO' c) -> (a -> IO' c)
(>=>) m1 m2 = outArg $ inArg m1 >>> inArg m2

arr' :: (a -> b) -> (a -> IO' b)
arr' = outArg . arr
