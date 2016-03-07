data F = F { app :: F -> F } | S { str :: String }

(.$.) :: F -> F -> F
(.$.) = app

s, k, i :: F
s = F $ \x -> F $ \y -> F $ \z -> x .$. z .$. (y .$. z)
k = F $ \x -> F $ \y -> x
i = s .$. k .$. k
