newtype I a = I { unI :: I a -> a }

s x y z = x z (y z)

m :: I a -> a
m = s unI id

b = (.)

c = flip

l = c b m

y = s l (b I l)

fact _ 0 = 1
fact f n = n * f (n - 1)
