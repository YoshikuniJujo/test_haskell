{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module LangYj () where

import Messages

instance MessageLang "yj" where
	whatsYourName = "hogehoge piyopiyo"
	hello n = "mogemoge " ++ n ++ "!"
	goodBye = "mugumugu!"
