# haskell-2020
Material for the DL Haskell 2020 class

# The Text
- primarily [Programming in Haskell](https://www.amazon.com/Programming-Haskell-Graham-Hutton/dp/1316626229/ref=asc_df_1316626229/?tag=hyprod-20&linkCode=df0&hvadid=312128454859&hvpos=&hvnetw=g&hvrand=6305069334589622472&hvpone=&hvptwo=&hvqmt=&hvdev=c&hvdvcmdl=&hvlocint=&hvlocphy=1022588&hvtargid=pla-404766124279&psc=1)


# Getting started

## Installing
The simplest way to get started is installing ghcup [ghcup](https://www.haskell.org/ghcup/) with `curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`.

If the above gives you a difficult time there are other [options](https://www.haskell.org/downloads/#platform).

This gives you the following:
- GHC, [Glasgow Haskell Compiler](https://www.haskell.org/ghc/)
- Cabal, a build system which should generally be avoided like the plague. 
- Stack, a haskell project management / build system (almost a hazmat suit for using cabal).
- Other suff (profilers, code coverage, etc)

> note: make sure to have the following installed: `sudo apt upgrade build-essential curl libgmp-dev libffi-dev libncurses-dev libtinfo5`

## Hello World
here we go:

```shell
$ cd $(mktemp -d)
$ echo 'main = putStrLn "Hello World"' > main.hs
$ runghc main.hs
Hello World
```
Not too bad.

In the above example we evaluated `main.hs` using an interpreter. We can also compile haskell:
```shell
$ ghc main.hs 
$ ./main
Hello World
```

We can also interactivly execute haskell code using ghci, the haskell repl:
```
$ ghci main.hs
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( main.hs, interpreted )
Ok, one module loaded.
*Main> -- this is a haskell commnet
*Main> main -- run the main function
Hello World
```
