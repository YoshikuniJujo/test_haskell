memo
====

```
ssize_t readv(int fd, const struct iovec *iov, int iovcnt);
ssize_t writev(int fd, const struct iovec *iov, int iovcnt);
struct iovec {
	void *iov_base;
	size_t iov_len;
};
```

TODO
----

つぎにやること

* リファクタリング
	+ module Iovec
	+ module VectoredIo
	+ app/Main.hs
	+ app/highLevel.hs
