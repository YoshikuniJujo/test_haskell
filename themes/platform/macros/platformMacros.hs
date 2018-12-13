{-# LANGUAGE CPP #-}

#define foo "bar"

main :: IO ()
main = do
	putStrLn foo
	putStrLn HOST_OS
