{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

app :: Application
app request respond = respond $ case rawPathInfo request of
	rp -> function rp

function :: BS.ByteString -> Response
function req = responseLBS
	undefined
	[("Content-Type", "text/plain")]
	(LBS.fromStrict req)
