import IOMcn

message :: Bool -> IOMcn String ()
message True = arr reverse >>> putLine
message False = putLine
