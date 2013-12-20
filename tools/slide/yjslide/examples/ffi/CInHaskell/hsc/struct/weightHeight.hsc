import Foreign.Marshal
import Foreign.C.String
import Foreign.Storable

main :: IO ()
main = allocaBytes #{size body} $ \ptr -> do
	#{poke body, name} ptr =<< newCString "Tarou"
	#{poke body, weight} ptr (72.5 :: #{type double})
	#{poke body, height} ptr (182.5 :: #{type double})
	putStrLn =<< peekCString =<< #{peek body, name} ptr
	print =<< (#{peek body, weight} ptr :: IO #{type double})
	print =<< (#{peek body, height} ptr :: IO #{type double})

#{def

typedef struct body {
	char *name;
	double weight;
	double height;
} body;

}
