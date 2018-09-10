import System.Info

main :: IO ()
main = do
	putStrLn os
	putStrLn arch
	putStrLn compilerName
	print compilerVersion
