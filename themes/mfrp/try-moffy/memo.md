memo
====

todo
----

* [x] parallel freer
* [x] adjust
* [x] check leftDown \`first\` rightUp
* [x] `update :: React s es a -> ... -> React s es' b -> ...` -> `update :: React s es a -> ... -> React s es b -> ...`
	+ adjustしてからupdateする感じか
* [x] DeleteEventでちゃんと終了するようにする

ref
---

```
themes/papers/monadic_functional_reactive_programming/try-monadic-functional-reactive-programming/
```
task
----

* [ ] qAppParで再帰的にタグをつけるのをやめる
	+ [ ] 明示的にタグづけしたレベルだけ効率化する
* [ ] タグづけした部分ではThreadIdはうまく働かないことを確かめる
