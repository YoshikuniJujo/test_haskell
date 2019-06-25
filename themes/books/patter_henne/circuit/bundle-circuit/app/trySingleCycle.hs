{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Circuit
import Memory
import TryInstControl

main :: IO ()
main = print . take 2000
	$ peekMultOWires (rrfAllOutputs trySingleCycleRrf)
		<$> iterate step trySingleCycleCct
