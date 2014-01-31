import IOMcn

message :: Bool -> IOMcn String ()
message True = arr reverse >>> putLine
message False = putLine

sayHello :: Bool -> (IOMcn String (), String)
sayHello b = (message b, "hello")

greeting :: IOMcn () ()
greeting = isEven >>> arr sayHello >>> app
