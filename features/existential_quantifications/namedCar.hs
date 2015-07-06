{-# LANGUAGE ExistentialQuantification #-}

class CarClass c where
	showCar :: c -> String

data Car = forall c . CarClass c => Car c

instance CarClass Car where
	showCar (Car c) = showCar c

data NamedCar = NamedCar String

instance CarClass NamedCar where
	showCar (NamedCar n) = n

data NamedCarWithColorName = NamedCarWithColorName String String

instance CarClass NamedCarWithColorName where
	showCar (NamedCarWithColorName n c) = n ++ " " ++ c

data NamedCarWithColorNameAndEngineType = NamedCarWithColorNameAndEngineType String String String

instance CarClass NamedCarWithColorNameAndEngineType where
	showCar (NamedCarWithColorNameAndEngineType n c e) = n ++ " " ++ c ++ " " ++ e

cars :: [Car]
cars = [Car (NamedCar "Lexus"), Car (NamedCarWithColorName "Crown" "Pink"), Car (NamedCarWithColorNameAndEngineType "M3" "Black" "M88/3")]

main :: IO ()
main = mapM_ (putStrLn . showCar) cars
