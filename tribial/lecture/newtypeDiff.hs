data DT = DT Int deriving Show

newtype NT = NT Int deriving Show

checkDT :: DT -> String
checkDT (DT _) = "OK!"

checkNT :: NT -> String
checkNT (NT _) = "OK!"
