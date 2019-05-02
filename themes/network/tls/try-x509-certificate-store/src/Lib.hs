{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.Maybe
import Data.X509.CertificateStore
import Paths_try_x509_certificate_store

getMyCertStore :: IO CertificateStore
getMyCertStore =
	(fromJust <$>) $ readCertificateStore =<< getDataFileName "cacert.pem"
