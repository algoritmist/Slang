# Σλang
Σλang is a simple and yet powerful core language that can be used as an interpreter for other languages
## Done:
1. Basic expression compiler
1. Parser
1. Laziness
## TODO:
1. case & let expressions
2. more primitives
3. data structures (list, etc.)
## Build
**Tested on Ubuntu**

You need `git`, `ghc`, `stack` packages to build Σλang</br>
Clone project:
```
git clone github.com/algoritmist/Slang.git
cd Slang
stack build
```
Now you can run it:
```stack exec -i <source_file.sl>```