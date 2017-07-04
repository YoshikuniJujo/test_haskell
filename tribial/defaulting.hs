default Some (())

class Some a where
	hige :: a -> String

instance Some () where
	hige = const "hello"
