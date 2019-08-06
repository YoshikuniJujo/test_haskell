{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

join' :: IO (IO a) -> IO a
join' io = do
	nxt <- io
	nxt

join4 :: IO (IO (IO (IO a))) -> IO a
join4 io = do
	io2 <- io
	io3 <- io2
	io4 <- io3
	io4
