{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Next where

sayHelloTo :: String -> String
sayHelloTo = (++ "!") . ("Hello, " ++)

sayHelloTo' :: forall a . SayHelloTo a => String
sayHelloTo' = "Hello, " ++ sayHelloToWho @a ++ "!"

class SayHelloTo a where
	sayHelloToWho :: String

data Yoshio

instance SayHelloTo Yoshio where
	sayHelloToWho = "Yoshio"

data Yasuo

instance SayHelloTo Yasuo where
	sayHelloToWho = "Yasuo"

data Bucciarati

instance SayHelloTo Bucciarati where
	sayHelloToWho = "Bruno"

data Narancia

instance SayHelloTo Narancia where
	sayHelloToWho = "Narancia"
