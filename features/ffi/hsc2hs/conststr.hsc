#define HELLO "world"

main :: IO ()
main = do
	print #const HELLO
	print #const_str HELLO
