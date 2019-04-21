{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib (getMyCertStore) where

import Data.Maybe
import Data.X509.CertificateStore
import Paths_helloworld_client_auth

getMyCertStore :: IO CertificateStore
getMyCertStore =
	(fromJust <$>) $ readCertificateStore =<< getDataFileName "cacert.pem"
