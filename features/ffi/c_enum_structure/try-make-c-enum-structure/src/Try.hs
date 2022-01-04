{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try where

import Language.C.Data.Position
import Language.C.Data.InputStream
import Language.C.Data.Ident
import "language-c" Language.C.Parser
import Language.C.System.Preprocess
import Language.C.System.GCC
import Data.Maybe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Language.C.Syntax.AST as A

parseCFile :: FilePath -> IO (Either ParseError A.CTranslUnit)
parseCFile fp = (`parseC` initPos fp) <$> BS.readFile fp

removeSharpLine :: InputStream -> InputStream
removeSharpLine = BSC.unlines . filter (not . ("#" `BS.isPrefixOf`)) . BSC.lines

parseExample :: IO (Either ParseError A.CTranslUnit)
parseExample = do
	Right istr <- runPreprocessor (newGCC "gcc") $ cppFile "/usr/include/vulkan/vulkan_core.h"
	let	istr' = removeSharpLine istr
	pure . parseC istr' $ initPos "foo"

data CEnum = CEnum { enumType :: Maybe String, enumElems :: [String] }
	deriving Show

cTranslUnitToEnums :: A.CTranslUnit -> [CEnum]
cTranslUnitToEnums = cTranslationUnitMapCTypeSpec cTypeSpecifierToEnum

cTranslationUnitMapCTypeSpec ::
	(A.CTypeSpecifier a -> Maybe x) -> A.CTranslationUnit a -> [x]
cTranslationUnitMapCTypeSpec f (A.CTranslUnit eds _) =
	cExternalDeclarationMapCTypeSpec f =<< eds

cExternalDeclarationMapCTypeSpec ::
	(A.CTypeSpecifier a -> Maybe x) -> A.CExternalDeclaration a -> [x]
cExternalDeclarationMapCTypeSpec f = \case
	A.CDeclExt d -> cDeclarationMapCTypeSpec f d
	_ -> []

cDeclarationMapCTypeSpec ::
	(A.CTypeSpecifier a -> Maybe x) -> A.CDeclaration a -> [x]
cDeclarationMapCTypeSpec f = \case
	A.CDecl dss _ _ -> cDeclarationSpecifierMapCTypeSpec f `mapMaybe` dss
	_ -> []

cDeclarationSpecifierMapCTypeSpec ::
	(A.CTypeSpecifier a -> Maybe x) -> A.CDeclarationSpecifier a -> Maybe x
cDeclarationSpecifierMapCTypeSpec f = \case
	A.CTypeSpec ts -> f ts
	_ -> Nothing

cTypeSpecifierToEnum :: A.CTypeSpecifier a -> Maybe CEnum
cTypeSpecifierToEnum = \case
	A.CEnumType et _ -> Just $ cEnumerationToEnum et
	_ -> Nothing

cEnumerationToEnum :: A.CEnumeration a -> Try.CEnum
cEnumerationToEnum (A.CEnum mtn (Just es) _ _) =
	Try.CEnum (identToName <$> mtn) (identToName . fst <$> es)
cEnumerationToEnum (A.CEnum mtn Nothing _ _) = Try.CEnum (identToName <$> mtn) []

data CStruct = CStruct (Maybe String) [(String, String)] deriving Show

cStructureUnionToStruct :: A.CStructureUnion a -> Maybe CStruct
cStructureUnionToStruct = \case
	A.CStruct A.CStructTag mtn (Just ds) _ _ ->
		Just $ CStruct (identToName <$> mtn) []
	A.CStruct A.CStructTag mtn Nothing _ _ ->
		Just $ CStruct (identToName <$> mtn) []
	_ -> Nothing

identToName :: Ident -> String
identToName (Ident nm _ _) = nm
