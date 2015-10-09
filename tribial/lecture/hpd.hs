data Human = Human String Int deriving Show

masuo :: Human
masuo = Human "Masuo" 32

age :: Human -> String
age (Human n a) = n ++ " is " ++ show a ++ " years old."

data Product = Product String Int deriving Show

iphone6s :: Product
iphone6s = Product "iPhone 6s" 99000

price :: Product -> String
price (Product n p) = n ++ " is " ++ show p ++ " yen."
