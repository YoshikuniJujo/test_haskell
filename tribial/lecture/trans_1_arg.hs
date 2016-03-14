import Data.Char (chr, ord)

myChr :: (a -> Int) -> (a -> Char)
myChr f = \x -> chr $ f x

addArg :: (b -> c) -> (a -> b) -> a -> c
addArg fun f x = fun (f x)

rmArg :: ((a -> b) -> a -> c) -> b -> c
rmArg fun' x = fun' (const x) undefined
