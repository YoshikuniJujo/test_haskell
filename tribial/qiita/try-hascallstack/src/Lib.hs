module Lib where

import GHC.Stack

someFunc :: IO ()
someFunc = putStrLn "someFunc"

f :: HasCallStack => IO ()
f = putStrLn (prettyCallStack callStack)

g :: HasCallStack => IO ()
g = f

h :: IO ()
h = f

foo :: HasCallStack => IO ()
foo = print callStack
-- foo = putStrLn $ prettyCallStack callStack

bar :: HasCallStack => IO ()
bar = foo

baz :: HasCallStack => IO ()
baz = bar

qux :: IO ()
qux = baz

myHead :: HasCallStack => [a] -> a
myHead [] = error "bad"
myHead (x : _) = x

useMyHead = myHead []

myHead' :: [a] -> a
myHead' [] = error "bad"
myHead' (x : _) = x

useMyHead' = myHead' []
