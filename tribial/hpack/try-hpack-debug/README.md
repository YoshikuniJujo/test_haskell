# try-hpack-debug

## UseCpp

```
% stack build
% stack exec try-hpack-debug-exe
foo is false
bar is true
% stack biuld --flag try-hpack-debug:foo
% stack exec try-hpack-debug-exe
foo is true
bar is true
% stack biuld --flag try-hpack-debug:foo --flag try-hpack-debug:-bar
foo is true
bar is false
```

## UseDebug

```
% stack build
% stack exec try-debug
Debug mode
% stack build --flag try-hpack-debug:-debug
% stack exec try-debug
No debug mode
```
