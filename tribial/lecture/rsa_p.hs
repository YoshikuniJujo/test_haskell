xxcrypt n ed mc = mc ^ ed `mod` n

xxcrypt1 = xxcrypt 138689

encrypt = xxcrypt1 13
decrypt = xxcrypt1 95497
