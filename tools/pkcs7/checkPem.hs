{-# LANGUAGE ScopedTypeVariables #-}

import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.X509
import Data.X509.File
import System.Environment

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.PubKey.RSA.Prim as RSA

main :: IO ()
main = do
	fp : _ <- getArgs
	(cs :: [SignedCertificate]) <- readSignedObject fp
	let	certs@[cert1, cert2, cert3] = map getSigned cs
		[PubKeyRSA pub1, PubKeyRSA pub2, PubKeyRSA pub3] =
			map (certPubKey . signedObject) certs
		rs@[objRaw1, objRaw2, objRaw3] = map getSignedData cs
		[objAsn1, objAsn2, objAsn3] =
			map (decodeASN1 DER . LBS.fromChunks . (: [])) rs
	mapM_ (print . showCN) certs
--	print objAsn2
	print $ SHA1.hash objRaw2
	print . RSA.ep pub1 $ signedSignature cert2

	putStrLn ""

--	print objAsn3
	print $ SHA1.hash objRaw3
	print $ signedSignature cert3
	print . RSA.ep pub2 $ signedSignature cert3

showCN :: Signed Certificate -> Maybe ASN1CharacterString
showCN = getDnElement DnCommonName . certSubjectDN . signedObject
