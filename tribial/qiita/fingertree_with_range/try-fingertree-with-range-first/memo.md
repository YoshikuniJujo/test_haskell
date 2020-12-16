memo
====

* [x] make Expression instance of Outputable
* [ ] refactoring Data.Derivation.Parse of try-finger-trees-ng

```
class Nodes m w where nodes :: Range 2 m a -> Range 1 w (Range 2 3 a)

instance Nodes 3 1 where nodes xs = [xs]

instance Nodes (m - 3) (w - 1) => Nodes m w where
	nodes [a, b] = [[a, b]]
	nodes [a, b, c] = [[a, b, c]]
	nodes [a, b, c, d] = [[a, b], [c, d]]
	nodes (a : b : c : xs@(_ : _ : _)) = [a, b, c] : nodes xs
```
