{-# LANGUAGE ScopedTypeVariables, TypeApplications, KindSignatures, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- module ProfileQueue (profileWhole) where
module ProfileQueue where

import Data.List (unfoldr)

import Queue

profileWhole :: forall q . Queue q => Int -> IO ()
profileWhole n = do
	putStrLn $ "make " ++ show n ++ " and " ++ show (n * 10) ++ " elem queues"
	print . sum $ {-# SCC "BUILD_AND_BREAK_N_ELEM_QUEUE" #-}
		unfoldr uncons ({-# SCC "BUILD_N_ELEM_QUEUE" #-} queueN @q n)
	print . sum $ {-# SCC "BUILD_AND_BREAK_N*10_ELEM_QUEUE" #-}
		unfoldr uncons ({-# SCC "BUILD_N*10_ELEM_QUEUE" #-} queueN @q (n * 10))

queueN :: Queue q => Int -> q Int
queueN n = snocAll empty [1 .. n]

queue1000 :: Queue q => q Int
queue1000 = queueN 1000

queue10000 :: Queue q => q Int
queue10000 = queueN 10000
