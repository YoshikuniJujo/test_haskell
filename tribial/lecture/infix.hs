(.+.), (.*.), (.-.) :: Int -> Int -> Int
(.+.) = (+)
(.*.) = (*)
(.-.) = (-)

infixl 6 .+., .-.

data A = B :+: B deriving Show

data B = Int :*: Int deriving Show

infixl 6 :+:
infixl 7 :*:
