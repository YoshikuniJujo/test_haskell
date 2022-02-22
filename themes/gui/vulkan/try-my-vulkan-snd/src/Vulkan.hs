{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Control.Monad.Cont
import Data.Word

import qualified Data.Text as T

import Vulkan.Base

import qualified Vulkan.Core as C
import qualified Vulkan.Instance.Core as Instance.C

newtype Instance = Instance Instance.C.Instance deriving Show

data ApplicationInfo a = ApplicationInfo {
	applicationInfoNext :: Maybe a,
	applicationInfoApplicationName :: T.Text,
	applicationInfoApplicationVersion :: ApiVersion,
	applicationInfoEngineName :: T.Text,
	applicationInfoEngineVersion :: ApiVersion,
	applicationInfoApiVersion :: ApiVersion }
	deriving Show

newtype ApiVersion = ApiVersion C.ApiVersion deriving (Show, Eq, Ord, Storable)

apiVersion_1_0 :: ApiVersion
apiVersion_1_0 = ApiVersion C.apiVersion_1_0

type Variant = Word8	-- 0 <= variant < 8
type Major = Word8	-- 0 <= major < 127
type Minor = Word16	-- 0 <= minor < 1023
type Patch = Word16	-- 0 <= patch < 4095

makeApiVersion :: Variant -> Major -> Minor -> Patch -> ApiVersion
makeApiVersion v mj mn p = ApiVersion $ C.makeApiVersion v mj mn p

applicationInfoToCore :: Pointable a =>
	ApplicationInfo a -> ContT r IO (Ptr C.ApplicationInfo)
applicationInfoToCore ApplicationInfo {
	applicationInfoNext = mnxt,
	applicationInfoApplicationName = anm,
	applicationInfoApplicationVersion = (\(ApiVersion v) -> v) -> appv,
	applicationInfoEngineName = enm,
	applicationInfoEngineVersion = (\(ApiVersion v) -> v) -> engv,
	applicationInfoApiVersion = (\(ApiVersion v) -> v) -> apiv
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	canm <- textToCString anm
	cenm <- textToCString enm
	let	C.ApplicationInfo_ fApplicationInfo = C.ApplicationInfo {
			C.applicationInfoSType = (),
			C.applicationInfoPNext = pnxt,
			C.applicationInfoPApplicationName = canm,
			C.applicationInfoApplicationVersion = appv,
			C.applicationInfoPEngineName = cenm,
			C.applicationInfoEngineVersion = engv,
			C.applicationInfoApiVersion = apiv }
	ContT $ withForeignPtr fApplicationInfo
