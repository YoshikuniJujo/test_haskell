data MyT = MyT Int Bool Char deriving Show

infix 2 :-<
infix 5 :-|
infix 8 :->

data WeekOp = Int :-< Bool deriving Show
data MiddleOp = WeekOp :-| StrongOp deriving Show
data StrongOp = Char :-> Double deriving Show

data RecOp = RecOp :- Int | Empty deriving Show
