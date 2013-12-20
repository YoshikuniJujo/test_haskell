import Foreign.Marshal
import Foreign.Storable
import Foreign.Ptr
import Data.Int

main :: IO ()
main = allocaBytes #{size seiseki} $ \ptr -> do
	#{poke seiseki, no} ptr (3 :: #type int)
	pokeArray0 '\NUL' (#{ptr seiseki, name} ptr) "Tarou"
	#{poke seiseki, average} ptr (80.5 :: #type double)
	print =<< (peekArray0 '\NUL' (#{ptr seiseki, name} ptr) :: IO String)

#{def

typedef struct seiheki {
	int no;
	char name[20];
	double average;
} seiseki;

}
