import Control.Concurrent

import Reactive.Threepenny

main :: IO ()
main = do
	(e, h) <- newEvent :: IO (Event Int, Handler Int)
	_ <- register e print
	_ <- register e $ \n -> print (n * 5)
	h 12345
	h 88559
	b <- stepper 0 e
	currentValue b >>= print
	h 54321
	currentValue b >>= print
