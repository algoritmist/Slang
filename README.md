# Σλang
Σλang is a simple and yet powerful core language that can be used as an interpreter for other languages
## Done:
1. Basic expression compiler
1. Parser
1. Laziness
## TODO:
1. let expressions
2. more primitives
3. data structures (list, etc.)
## Build
**Tested on Ubuntu**

You need `git`, `ghc`, `stack` packages to build Σλang</br>
Clone project:
```
git clone github.com/algoritmist/Slang.git
cd Slang
```
Now you can run it:
```stack build --exec "Slang <source_file.sl>"```

You can find examples of programms in directory `programs`

## Literature
1. ISBN-10: 0137219520 Peyton Jones, Simon & Lester, David. (1992). Implementing Functional Language – A Tutorial. 
2. Leijen, Daan. (2001). Parsec, a Fast Combinator Parser 
