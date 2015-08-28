import System.Environment
import System.Console.GetOpt

data Switch = Off | On deriving Show

main :: IO ()
main = do
	args <- getArgs
	case option args of
		Right (ss, as) -> putStrLn $
			"OPTIONS: " ++ show ss ++ "\n" ++
			"ARGS   : " ++ show as ++ "\n"
		Left err -> putStr err

option :: [String] -> Either String ([Switch], [String])
option args = case getOpt RequireOrder [
		Option ['0'] ["off"] (NoArg Off) "switch off",
		Option ['1'] ["on"] (NoArg On) "swith on" ]
	args of
	(ss, as, []) -> Right (ss, as)
	(_, _, es) -> Left $ concat es
