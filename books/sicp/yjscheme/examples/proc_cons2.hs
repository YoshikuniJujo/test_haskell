cons x y = \m -> m x y
car z = z $ \p _ -> p
cdr z = z $ \_ q -> q
