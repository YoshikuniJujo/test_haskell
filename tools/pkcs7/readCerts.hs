{-# LANGUAGE ScopedTypeVariables #-}

import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.X509
import Data.X509.File
import System.Environment
import Crypto.PubKey.HashDescr

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.PubKey.RSA.Prim as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA

main :: IO ()
main = do
	fp : _ <- getArgs
	(certs :: [SignedCertificate]) <- readSignedObject fp
	let	cert1 = getSigned $ certs !! 2
		cert2 = getSigned $ certs !! 1
		cert3 = getSigned $ certs !! 0
		PubKeyRSA pub2 = certPubKey $ signedObject cert2
		PubKeyRSA pub3 = certPubKey $ signedObject cert3
		objASN1 =
--			toASN1 (signedObject cert1) []
			Start Sequence : toASN1 (signedObject cert1) [End Sequence]
		objASN2 =
--			toASN1 (signedObject cert2) []
			Start Sequence : toASN1 (signedObject cert2) [End Sequence]
		objRaw1 = encodeASN1' DER objASN1
		objRaw2 = encodeASN1' DER objASN2
		objRaw2' = getSignedData $ certs !! 1
		Right objASN2' = decodeASN1 DER $ LBS.fromChunks [objRaw2']
		(cert2' :: Either String (Certificate, [ASN1]))  = fromASN1 objASN2'
		(cert2_ :: Either String (Certificate, [ASN1]))  = fromASN1 objASN2
--		objASN2' :: = either (Left . show) fromASN1 . decodeASN1 DER $
--			LBS.fromChunks [objRaw2']
--	print . flip toASN1 [] $ signedObject cert1
	print $ certIssuerDN (signedObject cert1)
		== certSubjectDN (signedObject cert2)
	print pub2
	print $ SHA256.hash objRaw1
	print $ SHA1.hash objRaw1
--	print objRaw
--	print . BS.length $ SHA1.hash objRaw
--	print $ signedSignature cert1
	print . RSA.ep pub2 $ signedSignature cert1
	print . RSA.verify (hashDescrSHA1) pub2 objRaw1 $ signedSignature cert1

	print $ certIssuerDN (signedObject cert2)
		== certSubjectDN (signedObject cert3)
	print $ SHA256.hash objRaw2
	print $ SHA1.hash objRaw2
	print $ SHA1.hash objRaw2'
	print . RSA.ep pub3 $ signedSignature cert2
	print . RSA.verify (hashDescrSHA1) pub3 objRaw2 $ signedSignature cert2

	print $ certIssuerDN (signedObject cert3)
		== certSubjectDN (signedObject cert3)

	let	t = 20
		d = 991
		l = BS.length objRaw2
		l' = BS.length objRaw2'
	print l; print l'
	print . BS.take t $ objRaw2
	print . BS.take t $ objRaw2'
	print $ BS.take t objRaw2 == BS.take t objRaw2'
	print $ BS.drop (l - d) objRaw2 == BS.drop (l' - d) objRaw2'
	let	x = 42
		y = 44
	print $ take x objASN2
	print $ take x objASN2'
	print $ take x objASN2 == take x objASN2'
	print $ drop y objASN2 == drop y objASN2'

	print . take 2 $ drop 42 objASN2
	print . take 2 $ drop 42 objASN2'

	print cert2_
--	print cert2'
--	print $ signedObject cert2 == cert2'

	print . take 10 $ objASN2
	print . take 10 $ objASN2'
