data Certificate = Certificate {
	tbsCertificate :: TBSCertificate,
	signatureAlgorithm :: AlgorithmIdentifier,
	signatureValue :: BitString
	} deriving Show

data TBSCertificate = TBSCertificate {
	version :: Version,
	serialNumber :: CertificateSerialNumber,
	signature :: AlgorithmIdentifier,
	issuer :: Name,
	validity :: Validity,
	subject :: Name,
	subjectPublicKeyInfo :: SubjectPublicKeyInfo,
	issuerUniqueID :: Maybe UniqueIdentifier,
	subjectUniqueID :: Maybe UniqueIdentifier,
	extensions :: [Extension]
	} deriving Show

data Version = V1 | V2 | V3 deriving Show

type CertificateSerialNumber = Integer

data Validity = Validity {
	notBefore :: Time,
	notAfter :: Time
	} deriving Show

data Time = TimeUTCTime UTCTime | TimeGeneralizedTime GeneralizedTime deriving Show

type UniqueIdentifier = BitString

data SubjectPublicKeyInfo = SubjectPublicKeyInfo {
	algorithm :: AlgorithmIdentifier,
	subjectPublicKey :: BitString
	} deriving Show

data Extension = Extension {
	extnID :: ObjectIdentifier,
	critical :: Bool,
	extnValue :: OctetString
	} deriving Show

data UTCTime = UTCTime deriving Show
data GeneralizedTime = GeneralizedTime deriving Show
data AlgorithmIdentifier = AlgorithmIdentifier deriving Show
data BitString = BitString deriving Show
data Name = Name deriving Show
data ObjectIdentifier = ObjectIdentifier deriving Show
data OctetString = OctetString deriving Show
