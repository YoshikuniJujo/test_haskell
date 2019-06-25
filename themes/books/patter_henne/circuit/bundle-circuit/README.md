# bundle-circuit

```
% stack build
% stack exec trySingleCycle
```

上記のように実行すると、以下のコードを実行している途中経過の
レジスタの値をリストとして表示する。
一世代前のCORE i7上のKVM上での実行で1分くらいかかる。

```
ld x10, 56(x15)
sd x1, 8(x2)
sub x30, x1, x2
add x15, x10, x15
beq x30, x31, 20
nop
nop
nop
nop
add x3, x1, x10
```

Haskell製。
ソースコードは、いまのところ、ぐちゃぐちゃ。
