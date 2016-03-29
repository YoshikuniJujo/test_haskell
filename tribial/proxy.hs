import Data.Proxy

double :: Num n => Proxy n -> n -> n
double p n = 2 * n `asProxyTypeOf` p
