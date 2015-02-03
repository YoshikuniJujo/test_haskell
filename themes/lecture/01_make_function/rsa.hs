encrypt n e m = m ^ e `mod` n
decrypt n d c = c ^ d `mod` n
