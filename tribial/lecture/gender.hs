data Gender = Female | Male

call :: (Gender, String) -> String
call (g, n) = (case g of Female -> "Mrs. "; _ -> "Mr. ") ++ n
