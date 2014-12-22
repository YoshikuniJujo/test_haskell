{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Data.X509
import Data.X509.Validation
import Data.X509.CertificateStore
import Network.PeyoTLS.ReadFile

main :: IO ()
main = do
	fps <- getArgs
	cc@(CertificateChain [c1, c2, c3]) <- readCertificateChain fps
	let	cs = myCS cc
	let emptyVC = ValidationCache
		(\_ _ _ -> return ValidationCacheUnknown)
		(\_ _ _ -> return ())
	print cc
	validateDefault cs emptyVC ("VeriSign Class 3 Code Signing 2010 CA", "") (myCC cc) >>= print
--	validateDefault cs emptyVC ("Yahoo! Inc.", "") (myCC2 cc) >>= print
--	validateDefault cs emptyVC ("Yahoo! Inc.", "") cc >>= print
	print . getDnElement DnCommonName . certSubjectDN . signedObject $ getSigned c3
	print . getDnElement DnCommonName . certSubjectDN . signedObject $ getSigned c2
	print . getDnElement DnCommonName . certSubjectDN . signedObject $ getSigned c1

reverseCC, headCC, tailCC, myCC :: CertificateChain -> CertificateChain
reverseCC (CertificateChain cc) = CertificateChain $ reverse cc
headCC (CertificateChain cc) = CertificateChain [head cc]
tailCC (CertificateChain cc) = CertificateChain $ tail cc
appendCC (CertificateChain cc1) (CertificateChain cc2) = CertificateChain $ cc1 ++ cc2

myCC (CertificateChain [c1, c2, c3]) = CertificateChain [c2]
myCC2 (CertificateChain [c1, c2, c3]) = CertificateChain [c3, c2, c1]
myCS (CertificateChain [c1, c2, c3]) = makeCertificateStore [c1]
