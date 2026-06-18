import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Array qualified as JS.Array

main :: IO ()
main = do
	JS.Value.consoleLog . JS.Value.toV =<< JS.Array.fromList [3 :: Int .. 15]
