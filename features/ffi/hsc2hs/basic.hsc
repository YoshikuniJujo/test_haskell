#define HELLO 888

(##) :: Int -> Int -> Int
(##) = (+)

main :: IO (#define WORLD 999)
main = do
	pr#{define YOU 111}int #const HELLO
	print $ #{const WORLD} ## #const YOU
