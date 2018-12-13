import SysTools

main :: IO ()
main = do
	initSysTools (Just ".") >>= print
