import Test.HUnit
import Data.Time
import Data.Char

main = runTestTT $ "test" ~: [
	True ~? "True",
	False ~? "False",
	"3 + 5 should be 9" ~: 9 ~=? 3 + 5,
	3 + 5 ~?= 9,
	"some" ~: assertString "hoge",
	"hoge" ~: do
		assertFailure "",
	"time" ~: do
		t1 <- getCurrentTime
		t2 <- getCurrentTime
		t1 @=? t2
		t1 <- getCurrentTime
		t2 <- getCurrentTime
		t2 @?= t1,
	"False" ~: do
		True @? "True"
		(getCurrentTime >> return False) @? "False",
	isAscii 'あ' ~? "'あ' is not ASCII",
	"'3 + 5' is expected to be 9" ~: 9 ~=? 3 + 5
 ]
