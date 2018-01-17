# Haskell Fundamentals
Learning project for haskell

## How this project was created
- brew install haskell-stack
- stack new haskell-fundamentals
- cd haskell-fundamentals
- stack test
- git init
- git add --all
- git commit -m "Created haskell-fundamentals project"
- git remote add origin git@gitlab.cj.com:cjdev/haskell-fundamentals.git
- git push -u origin master

## How to launch a particular entry point from the command line
- choose in entry point to run, for example
    - src/HelloAlice.hs
- run it as a script, like so
    - stack runhaskell src/HelloAlice.hs

## How to set up test monitoring
stack test --file-watch

## Fundamentals explained through sample code
- [Functions](test/FunctionSpec.hs)
- [Data](test/DataSpec.hs)
- [Collection](test/CollectionSpec.hs)
- [Tuples](test/TuplesSpec.hs)
- [Flow Control](test/FlowControlSpec.hs)
- [Maybe](test/MaybeSpec.hs)
- [Either](test/EitherSpec.hs)
- [Fold and Tail Recursion](test/TypesOfLoopsSpec.hs)
- [Functor](test/FunctorSpec.hs)
- [Applicative](test/ApplicativeSpec.hs)
- [Monad](test/MonadSpec.hs)
- [Monoid](test/MonoidSpec.hs)
- Test with freer — [test](test/Maintainability/Freer/MainSpec.hs), [stubs](test/Maintainability/Freer/Stubs.hs)
- Test with mtl — [test](test/Maintainability/MTL/MainSpec.hs), [stubs](test/Maintainability/MTL/Stubs.hs)
- [Parser Combinators](test/ParserCombinatorSpec.hs)
- [Validation](test/ValidationSpec.hs)
- [List Comprehension](test/ListComprehensionSpec.hs)
- [Laziness](test/LazinessSpec.hs)

## Some possible settings for Visual Studio Code
    {
      "window.zoomLevel": 2,
      "editor.tabSize": 2,
      "editor.detectIndentation": false,
      "files.trimTrailingWhitespace": true
    }
