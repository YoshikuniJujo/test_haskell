# try-hpack-debug

```
# stack build
# stack exec try-hpack-debug-exe
foo is false
bar is true
# stack biuld --flag try-hpack-debug:foo
# stack exec try-hpack-debug-exe
foo is true
bar is true
# stack biuld --flag try-hpack-debug:foo --flag try-hpack-debug:-bar
foo is true
bar is false
```
