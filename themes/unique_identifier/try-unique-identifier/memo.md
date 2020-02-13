memo
====

data It i a = Get deriving Show

type Iteratee i = Freer (It i)

data Freer t a = Pure a | forall x . t x \`Bind\` FTCQueue (Freer t) x a
