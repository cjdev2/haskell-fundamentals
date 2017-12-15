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
- [List Comprehension](test/ListComprehensionSpec.hs)
- [Maybe](test/MaybeSpec.hs)
- Functor
- Applicitive
- Monad
- Monoid
- Parser Combinators
- Either
- Validation
