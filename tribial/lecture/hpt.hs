type Human = (String, Int)

masuo :: Human
masuo = ("Masuo", 32)

age :: Human -> String
age (n, a) = n ++ " is " ++ show a ++ " years old."

type Product = (String, Int)

iphone6s :: Product
iphone6s = ("iPhone 6s", 99000)

price :: Product -> String
price (n, p) = n ++ " is " ++ show p ++ " yen."
