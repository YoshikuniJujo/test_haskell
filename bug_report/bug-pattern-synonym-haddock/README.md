bug-pattern-synonym-haddock
============================

```
% stack haddock
bug-pattern-synonym-haddock> configure (lib)
Configuring bug-pattern-synonym-haddock-0.1.0.0...

Warning: Failed to decode module interface:
         /home/tatsuya/projects/test_haskell/bug_report/bug-pattern-synonym-haddock/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.0.0/build/A.hi
         Decoding failure: Invalid magic: e49ceb0f

Warning: Failed to decode module interface:
         /home/tatsuya/projects/test_haskell/bug_report/bug-pattern-synonym-haddock/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.0.0/build/B.hi
         Decoding failure: Invalid magic: e49ceb0f
bug-pattern-synonym-haddock> build (lib)
Preprocessing library for bug-pattern-synonym-haddock-0.1.0.0..
Building library for bug-pattern-synonym-haddock-0.1.0.0..

Warning: Failed to decode module interface:
         /home/tatsuya/projects/test_haskell/bug_report/bug-pattern-synonym-haddock/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.0.0/build/A.hi
         Decoding failure: Invalid magic: e49ceb0f

Warning: Failed to decode module interface:
         /home/tatsuya/projects/test_haskell/bug_report/bug-pattern-synonym-haddock/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.0.0/build/B.hi
         Decoding failure: Invalid magic: e49ceb0f
bug-pattern-synonym-haddock> haddock
Preprocessing library for bug-pattern-synonym-haddock-0.1.0.0..
Running Haddock on library for bug-pattern-synonym-haddock-0.1.0.0..
Haddock coverage:
   0% (  0 /  2) in 'B'
  Missing documentation for:
    Module header
    BarUnit (src/B.hs:6)
haddock: panic! (the 'impossible' happened)
  (GHC version 8.10.1:
	extractDecl

Ambiguous decl for BarUnit in class:
    class Foo a where
      data Bar a
Matches:
    [] []
Call stack:
    CallStack (from HasCallStack):
      callStackDoc, called at compiler/utils/Outputable.hs:1179:37 in ghc:Outputable
      pprPanic, called at utils/haddock/haddock-api/src/Haddock/Interface/Create.hs:1078:16 in main:Haddock.Interface.Create

Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug


--  While building package bug-pattern-synonym-haddock-0.1.0.0 using:
      /home/tatsuya/.stack/setup-exe-cache/x86_64-linux-tinfo6/Cabal-simple_mPHDZzAJ_3.2.0.0_ghc-8.10.1 --builddir=.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.0.0 haddock --html --hoogle --html-location=../$pkg-$version/ --haddock-option=--hyperlinked-source --haddock-option=--quickjump
    Process exited with code: ExitFailure 1
```
