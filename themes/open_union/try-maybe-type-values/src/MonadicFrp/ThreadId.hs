{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.ThreadId where

import Data.Bits

data ThreadId = ThreadId Word Integer deriving (Show, Eq)

rootThreadId :: ThreadId
rootThreadId = ThreadId 0 0

forkThreadId :: ThreadId -> (ThreadId, ThreadId)
forkThreadId (ThreadId n i) =
	(ThreadId n $ i + 1, ThreadId (n `setBit` fromIntegral i) $ i + 1)
