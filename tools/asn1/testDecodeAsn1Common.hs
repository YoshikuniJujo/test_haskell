{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import System.IO.Unsafe

import qualified Data.ByteString as BS

import DecodeAsn1Common

import Data.X509
import Network.PeyoTLS.ReadFile

import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.Prim as RSA
import qualified Crypto.Hash.SHA1 as SHA1

cert :: BS.ByteString
cert = unsafePerformIO $ BS.readFile "test_ASN_1_cert.der"

under30Error, redundantTagNumberError :: BS.ByteString
under30Error = "\x1f\x1e"
redundantTagNumberError = "\x1f\x80\x7f"

main :: IO ()
main = print $ runAnalyzer decodeTag cert

signed_a, sign1_a :: Asn1
Right (Asn1 _ (Asn1DataAsn1 [signed_a, _, sign1_a]) , "") =
	runAnalyzer (decode1 $ Rec1 [NoRec, RecAll, RecAll]) cert

sign1 :: BS.ByteString
Asn1 _ (Asn1DataRaw sign1) = sign1_a

CertificateChain [cacert] = unsafePerformIO $ readCertificateChain ["cacert.pem"]

pubkey :: RSA.PublicKey
PubKeyRSA pubkey = certPubKey . signedObject $ getSigned cacert

vr :: BS.ByteString
vr = RSA.ep pubkey sign1

Asn1 _ (Asn1DataRaw ss) = signed_a

hs :: BS.ByteString
hs = SHA1.hash $ "\x30\x82\x02\xb7" `BS.append` ss
