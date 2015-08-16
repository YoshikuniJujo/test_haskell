data R a = R { unR :: R a -> a }

y = \f -> (\x -> f (unR x x)) $ R (\x -> f (unR x x))

fact _ 0 = 1
fact f n = n * f (n - 1)
