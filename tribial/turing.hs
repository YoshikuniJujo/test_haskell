{-# LANGUAGE TypeFamilies #-}

data TapeAlphabet a t = A a | T t | Empty

class TState t where
	type Alphabet t
	type TAlphabet t
