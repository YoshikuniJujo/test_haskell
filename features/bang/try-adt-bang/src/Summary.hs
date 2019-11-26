module Summary where

import GHC.Exts.Heap

data Foo = Foo String deriving Show
data Bar = Bar !String deriving Show

foo, foo' :: Foo -> Foo
foo (Foo s) = Foo $ 'c' : s
foo' (Foo s) = Foo $! 'c' : s

bar :: Bar -> Bar
bar (Bar s) = Bar $ 'c' : s

checkFoo :: Foo -> IO Closure
checkFoo (Foo s) = getClosureData s

checkBar :: Bar -> IO Closure
checkBar (Bar s) = getClosureData s

data List a = Empty | Cons a (List a) deriving Show

data Baz = Baz (List Char) deriving Show

baz, baz' :: Baz -> Baz
baz (Baz s) = Baz $ Cons 'c' s
baz' (Baz s) = Baz $! Cons 'c' s

checkBaz :: Baz -> IO Closure
checkBaz (Baz s) = getClosureData s
