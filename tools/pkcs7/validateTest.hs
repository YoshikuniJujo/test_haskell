{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Data.X509
import Data.X509.Validation
import Data.X509.CertificateStore
import Network.PeyoTLS.ReadFile
import Crypto.PubKey.HashDescr

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.PubKey.RSA.Prim as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA

main :: IO ()
main = do
	fps <- getArgs
	CertificateChain [c1, c2, c3] <- readCertificateChain fps
	print . getDnElement DnCommonName . certSubjectDN . signedObject $ getSigned c1
	print . certIssuerDN . signedObject $ getSigned c1
	print . getDnElement DnCommonName . certSubjectDN . signedObject $ getSigned c2
	print . getDnElement DnCommonName . certIssuerDN . signedObject $ getSigned c2
	print . getDnElement DnCommonName . certSubjectDN . signedObject $ getSigned c3
	print . getDnElement DnCommonName . certIssuerDN . signedObject $ getSigned c3
	let	emptyVC = ValidationCache
			(\_ _ _ -> return ValidationCacheUnknown)
			(\_ _ _ -> return ())
		cs = makeCertificateStore [c1]
--		cs = makeCertificateStore []
		cc = CertificateChain [c2]
--	validateDefault cs emptyVC ("VeriSign Class 3 Code Signing 2010 CA", "") cc >>= print
	print $ certIssuerDN (signedObject $ getSigned c2)
		== certSubjectDN (signedObject $ getSigned c2)
	validate HashSHA1 defaultHooks defaultChecks cs emptyVC ("VeriSign Class 3 Code Signing 2010 CA", "") cc >>= print
	validate HashSHA256 defaultHooks defaultChecks cs emptyVC ("VeriSign Class 3 Code Signing 2010 CA", "") cc >>= print
	let	PubKeyRSA pub = certPubKey $ getCertificate c1
		alg = signedAlg $ getSigned c2
		dat = getSignedData c2
		sgn = signedSignature $ getSigned c2
	print alg
	print pub
	print dat
	print sgn
	print $ RSA.verify hashDescrSHA1 pub dat sgn
	print $ SHA1.hash dat
	print $ RSA.ep pub sgn
