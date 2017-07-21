tryIt :: Int -> Int
tryIt = (* 3)

class Some a where
	doSomething :: a -> a

instance Some Int where
	doSomething = (* 3)
