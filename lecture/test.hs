import Prelude hiding (div, mod)

result = some 8

some = \x -> add x 9

add = \x -> \y -> zero x y (add (pred x) (succ y))

zero 0 f _ = f
zero _ _ g = g
