xxcrypt n ed mc = mc ^ ed `mod` n

encrypt1 m = xxcrypt 138689 13 m
decrypt1 c = xxcrypt 138689 95497 c

encrypt1' = xxcrypt 138689 13
decrypt1' = xxcrypt 138689 95497

xxcrypt1 = xxcrypt 138689

encrypt1'' = xxcrypt1 13
decrypt1'' = xxcrypt1 95497
