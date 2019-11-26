module Lib where

import GHC.Exts.Heap

type Foo = Int
newtype Bar = Bar { getBar :: Int } deriving Show
data Baz = Baz { getBaz :: Int } deriving Show

-- newtype Qux = Qux !Int deriving Show
data Quux = Quux { getQuux :: !Int } deriving Show

checkBar :: Bar -> IO Closure
checkBar (Bar n) = getClosureData n

checkBaz :: Baz -> IO Closure
checkBaz (Baz n) = getClosureData n

checkQuux :: Quux -> IO Closure
checkQuux (Quux n) = getClosureData n

newtype Corge = Corge String deriving Show
data Grault = Grault String deriving Show
data Garply = Garply !String deriving Show

checkCorge :: Corge -> IO Closure
checkCorge (Corge ns) = getClosureData ns

checkGrault :: Grault -> IO Closure
checkGrault (Grault ns) = getClosureData ns

checkGarply :: Garply -> IO Closure
checkGarply (Garply ns) = getClosureData ns

data Waldo = Waldo String String deriving Show
data Fred = Fred String !String deriving Show

waldo :: Waldo -> Waldo
waldo (Waldo (x : f) r) = Waldo f (x : r)
waldo w = w

fred :: Fred -> Fred
fred (Fred (x : f) r) = Fred f (x : r)
fred f = f

checkWaldo :: Waldo -> IO Closure
-- checkWaldo (Waldo f r) = f `seq` getClosureData r
checkWaldo (Waldo f r) = print f >> getClosureData r
-- checkWaldo (Waldo f r) = print f >> (getClosureData $! r)

checkFred :: Fred -> IO Closure
-- checkFred (Fred f r) = f `seq` getClosureData r
checkFred (Fred f r) = getClosureData r
