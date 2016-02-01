module Machine (Machine, next, putHello, putWorld) where

newtype Machine = Machine { runMachine :: IO () }

putHello, putWorld :: Machine
putHello = Machine $ putStrLn "Hello"
putWorld = Machine $ putStrLn "World"

next :: Machine -> Machine -> Machine
next (Machine m1) (Machine m2) = Machine $ m1 >> m2
