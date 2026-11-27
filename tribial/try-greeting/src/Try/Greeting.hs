module Try.Greeting (greeting) where

greeting :: String -> String
greeting nm = "Hello, " ++ nm ++ "!"

foreign import javascript unsafe "() => console.log('FooBar');"
	foobar :: IO ()
