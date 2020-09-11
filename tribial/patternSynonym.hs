{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

pattern Foo :: Int
pattern Foo = 123

pattern Bar :: Int
pattern Bar <- 321

pattern Baz :: Int
pattern Baz <- 333
	where Baz = 333

qux :: Int
qux = 555

pattern Qux :: Int
pattern Qux <- ((== qux) -> True)
	where Qux = qux
