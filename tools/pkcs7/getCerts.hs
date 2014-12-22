import Control.Applicative
import Data.ASN1.Types
import Data.ASN1.BitArray
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import System.Environment

import qualified Data.ByteString.Lazy as LBS

import Asn1Container

main :: IO ()
main = do
	fp : args <- getArgs
	Right cnt <- decodeASN1 BER <$> LBS.readFile fp
	let	([CntSequence [oid0, CntContext 0 [bd0]]], []) =
			parseAsn1Container cnt
		CntSequence [i0, oid1, oid2, CntContext 0 crts, sgn] = bd0
		[crt0, crt1, crt2] = map (\(CntSequence cs) -> cs) crts
		str = case args of
			["others"] ->
				show oid0 ++ "\n" ++ show i0 ++ "\n" ++
				show oid1 ++ "\n" ++ show oid2 ++ "\n"
			["all_certs"] -> show (writeAsn1Container crts) ++ "\n"
			["certs", "0", cmd, sub] -> function cmd sub crt0 ++ "\n"
			["certs", "1", cmd, sub] -> function cmd sub crt1 ++ "\n"
			["certs", "2", cmd, sub] -> function cmd sub crt2 ++ "\n"
			["sign"] -> show sgn
			_ -> ""
	putStr str

function :: String -> String -> [Asn1Container] -> String
function "cert" n [CntSequence crt, _, _] = show $ crt !! read n
function "alg" _ [_, CntSequence alg, _] =
	unwords $ map (show . \(CntAtom a) -> a) alg
function "sign" _ [_, _, CntAtom (BitString (BitArray n sgn))] =
	show sgn ++ "(" ++ show n ++ ")"
function "whole" _ cs = show cs
function _ _ cs = show cs
