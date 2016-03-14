valToFun :: a -> ((a -> b) -> b)
valToFun x = ($ x)

funToVal :: ((a -> a) -> a) -> a
funToVal = ($ id)
