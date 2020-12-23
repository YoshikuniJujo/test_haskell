main :: IO ()
main = putStrLn "Test suite not yet implemented"

exampleBitsR :: LengthR 32 Bit
exampleBitsR =
	(repeatR I :: LengthR 8 Bit) +++ (repeatR O :: LengthR 8 Bit) +++
	(repeatR I :: LengthR 8 Bit) +++ (repeatR O :: LengthR 8 Bit)
