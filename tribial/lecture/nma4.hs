mret :: a -> Maybe a
mret = Just

mbind :: Maybe a -> (a -> Maybe b) -> Maybe b
-- mbind m f = maybe Nothing f m
mbind = flip $ maybe Nothing

mapp :: Maybe (a -> b) -> Maybe a -> Maybe b
mf `mapp` mx = mf `mbind` \f -> mx `mbind` \x -> mret $ f x

safeDivM :: Int -> Int -> Maybe Int
safeDivM _ 0 = Nothing
safeDivM x y = Just $ x `div` y

calcM :: Int -> Int -> Int -> Int -> Maybe Int
calcM a b c d = mret (+) `mapp` (a `safeDivM` b) `mapp` (c `safeDivM` d)

data TryM a = ErrorM String | SuccessM a deriving Show

tret :: a -> TryM a
tret = SuccessM

tbind :: TryM a -> (a -> TryM b) -> TryM b
SuccessM x `tbind` f = f x
ErrorM em `tbind` _ = ErrorM em

tmapp :: TryM (a -> b) -> TryM a -> TryM b
tf `tmapp` tx = tf `tbind` \f -> tx `tbind` \x -> tret $ f x

safeDivTM :: Int -> Int -> TryM Int
safeDivTM x 0 = ErrorM $ show x ++ " is divided by zero\n"
safeDivTM x y = SuccessM $ x `div` y

calcTM :: Int -> Int -> Int -> Int -> TryM Int
calcTM a b c d = tret (+) `tmapp` (a `safeDivTM` b) `tmapp` (c `safeDivTM` d)

data TryA a = ErrorA String | SuccessA a deriving Show

tpure :: a -> TryA a
tpure = SuccessA

tapp :: TryA (a -> b) -> TryA a -> TryA b
SuccessA f `tapp` SuccessA x = SuccessA $ f x
SuccessA _ `tapp` ErrorA em' = ErrorA em'
ErrorA em `tapp` SuccessA _ = ErrorA em
ErrorA em `tapp` ErrorA em' = ErrorA $ em ++ em'

safeDivTA :: Int -> Int -> TryA Int
safeDivTA x 0 = ErrorA $ show x ++ " is divided by zero\n"
safeDivTA x y = SuccessA $ x `div` y

calcTA :: Int -> Int -> Int -> Int -> TryA Int
calcTA a b c d = tpure (+) `tapp` (a `safeDivTA` b) `tapp` (c `safeDivTA` d)
