{-# LANGUAGE ScopedTypeVariables, KindSignatures #-}
{-# LANGUAGE TypeApplications, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ProfileQueue (profileWhole, queueN) where

import Data.List (unfoldr)

import Queue

profileWhole :: forall q . Queue q => Int -> IO ()
profileWhole n = do
	putStrLn $ show n ++ " and " ++ show (n * 10) ++ " elem queues"
	print . sum $ {-# SCC "BUILD_AND_BREAK_N_ELEM_QUEU" #-}
		unfoldr uncons (queueN @q n)
	print . sum $ {-# SCC "BUILD_AND_BREAK_N*10_ELEM_QUEUE" #-}
		unfoldr uncons (queueN @q (n * 10))

queueN :: Queue q => Int -> q Int
queueN n = snocAll empty [1 .. n]
