import ProcArray

main :: IO ()
main = do
	procArray 8 10 >>= print
	procArray 9 10 >>= print
	procArray 10 10 >>= print
	procArray 11 10 >>= print
	procArray 12 10 >>= print
