import Control.Applicative
import System.Environment
import System.Process
import System.Directory
import System.FilePath

main :: IO ()
main = do
	pre : pst : dir : _ <- getArgs
	fs <- map ((dir </>) . dropExtension)
		. filter ((== ('.' : pre)) . takeExtension)
			<$> getDirectoryContents dir
--	system $ "echo " ++ pre ++ " " ++ pst ++ " " ++ show fs
	mapM_ putStrLn $ map (mkCmd pre pst) fs
	mapM_ system $ map (mkCmd pre pst) fs
	return ()

mkCmd :: String -> String -> FilePath -> String
mkCmd pre pst bn = "git mv " ++ addExtension bn pre ++ " " ++ addExtension bn pst
