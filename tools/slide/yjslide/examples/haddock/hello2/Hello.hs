-- |greeting library show 'greeting'
-- 日本語はどうなのか
module Hello where

-- |make greeting
--
-- >>> greeting "world"
-- "Hello, world!"
greeting :: String -- ^your name
	-> String -- ^greeting
greeting name = "Hello, " ++ name ++ "!"
