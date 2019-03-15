{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Messages (MessageLang(..)) where

import GHC.Types

class MessageLang (l :: Symbol) where
	whatsYourName :: String
	hello :: String -> String
	goodBye :: String

instance MessageLang "en" where
	whatsYourName = "Waht's your name?"
	hello n = "Hello, " ++ n ++ "!"
	goodBye = "Good-bye!"

instance MessageLang "ja" where
	whatsYourName = "名前は何ですか?"
	hello n = "こんにちは、" ++ n ++ "!"
	goodBye = "さようなら!"
