{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MultiArgs where

profile :: String -> String -> Int -> String
profile g f a = g ++ " " ++ f ++ " (" ++ show a ++ ")"

profile' :: forall a . Profile a => String
profile' = given @a ++ " " ++ family @a ++ " (" ++ show (age @a) ++ ")"

class Profile p where
	given :: String
	family :: String
	age :: Int

data Tatsuya

instance Profile Tatsuya where
	given = "Tatsuya"
	family = "Yamashiro"
	age = 35

data Yoshio

instance Profile Yoshio where
	given = "Yoshio"
	family = "Joshita"
	age = 33

data Bucciarati

instance Profile Bucciarati where
	given = "Bruno"
	family = "Bucciarati"
	age = 20

data Narancia

instance Profile Narancia where
	given = "Narancia"
	family = "Girga"
	age = 17
