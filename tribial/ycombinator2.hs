data R a = R { unR :: R a -> a }

(.$.) = unR

y = \f -> (\x -> f $ x .$. x) . R $ \x -> f $ x .$. x

fact _ 0 = 1
fact f n = n * f (n - 1)
