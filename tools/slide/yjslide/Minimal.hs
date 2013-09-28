module Minimal where

data Number = Zero | One | Two | Three | Four | Five | Six | Error
	deriving Show

inc Zero = One
inc One = Two
inc Two = Three
inc Three = Four
inc Four = Five
inc Five = Six
inc Six = Error
inc Error = Error

dec Zero = Error
dec One = Zero
dec Two = One
dec Three = Two
dec Four = Three
dec Five = Four
dec Six = Five
dec Error = Error

add Error _ = Error
add _ Error = Error
add x Zero = x
add x y = inc (add x (dec y))

sub Error _ = Error
sub _ Error = Error
sub x Zero = x
sub x y = dec (sub x (dec y))

data Position = Position Number Number
	deriving Show

dif x y = case sub x y of
	Error -> sub y x
	s -> s

distance (Position x y) (Position x' y') =
	(x `dif` x') `add` (y `dif` y')
