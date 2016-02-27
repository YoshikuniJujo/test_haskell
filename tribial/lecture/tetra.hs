data Tetra = Tetra Bool T

instance Show Tetra where
	show (Tetra s t) = (if s then "-" else "") ++ show t

data T = Zero | One | Two | Three | Four

instance Show T where
	show Zero = "0"
	show One = "1"
	show Two = "2"
	show Three = "3"
	show Four = "4"

sccT, prdT :: T -> T
sccT Zero = One
sccT One = Two
sccT Two = Three
sccT Three = Four
sccT Four = Zero
prdT Zero = Four
prdT One = Zero
prdT Two = One
prdT Three = Two
prdT Four = Three

scc, prd :: Tetra -> Tetra
scc (Tetra False Four) = Tetra True Four
scc (Tetra False t) = Tetra False (sccT t)
scc (Tetra True Zero) = Tetra False One
scc (Tetra True One) = Tetra False Zero
scc (Tetra True t) = Tetra True (prdT t)
prd (Tetra s t) = Tetra (not s') t' where Tetra s' t' = scc $ Tetra (not s) t

instance Num Tetra where
	t1 + Tetra _ Zero = t1
	t1 + t2 = scc t1 + prd t2
	t1 * Tetra _ Zero = 0
	t1 * t2 = t1 + t1 * (prd t2)
	negate (Tetra s t) = Tetra (not s) t
	abs (Tetra _ t) = Tetra False t
	signum (Tetra _ Zero) = Tetra False Zero
	signum (Tetra False _) = Tetra False One
	signum (Tetra True _) = Tetra True One
	fromInteger i = case signum i of
		-1 -> iterate prd (Tetra False Zero) !! fromIntegral (abs i)
		_ -> iterate scc (Tetra False Zero) !! fromIntegral i
