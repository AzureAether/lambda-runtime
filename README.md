# A compiler and runtime for lambda calculus
This project is based around a runtime for a currently undocumented programming language based heavily on lambda calculus.

This runtime represents the program as a lambda term; there is also [a runtime which uses combinatory logic instead](https://github.com/bloootac/lambda-calculus-compiler).

Please see [the project webpage](https://www.skyshoesmith.com/projects/lambda-language).

## Getting set up
### Stuff you'll need
- [Alex](https://haskell-alex.readthedocs.io/en/latest/), a lexer generator for Haskell
- [Happy](https://haskell-happy.readthedocs.io/en/latest/), a parser generator for Haskell
- A way to compile Haskell, such as [GHC](https://www.haskell.org/ghc)
- A way to compile C, such as [gcc](https://gcc.gnu.org/)

### Build commands
```
alex lexer.x
happy parser.y
ghc -no-keep-hi-files -no-keep-o-files -main-is Lambda lambda.hs
```
