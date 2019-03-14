{-# LANGUAGE TypeApplications, KindSignatures, DataKinds, AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import GHC.Types

class Hello (a :: Nat) where hello :: String

instance Hello 1 where hello = "hello, world"
instance Hello 2 where hello = "Hello, world!"
instance Hello 3 where hello = "HELLO WORLD"
instance Hello 4 where hello = "Howdy, World!"
instance Hello 5 where hello = "こんにちは世界"

sayHello :: forall (a :: Nat) . Hello a => IO ()
sayHello = putStrLn $ hello @a
