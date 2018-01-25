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

## Language Extensions
- for example, to turn on overloaded strings
- in file
    - {-# LANGUAGE OverloadedStrings #-}
- in ghci
    - :set -XOverloadedStrings

## Fundamentals explained through sample code
- [Functions](test/FunctionSpec.hs)
- [Data](test/DataSpec.hs)
- [Collection](test/CollectionSpec.hs)
- [Tuples](test/TuplesSpec.hs)
- [Flow Control](test/FlowControlSpec.hs)
- [Maybe](test/MaybeSpec.hs)
- [Fold and Tail Recursion](test/TypesOfLoopsSpec.hs)
- [Functor](test/FunctorSpec.hs)
- [Applicative](test/ApplicativeSpec.hs)
- [Monad](test/MonadSpec.hs)
- [Semigroup](test/SemigroupSpec.hs)
- [Monoid](test/MonoidSpec.hs)
- Test with freer — [test](test/Maintainability/Freer/MainSpec.hs), [stubs](test/Maintainability/Freer/Stubs.hs)
- Test with mtl — [test](test/Maintainability/MTL/MainSpec.hs), [stubs](test/Maintainability/MTL/Stubs.hs)
- [Alternative](test/AlternativeSpec.hs)
- [Parser Combinators](test/ParserCombinatorSpec.hs)
- [Validation](test/ValidationSpec.hs)
- [List Comprehension](test/ListComprehensionSpec.hs)
- [Laziness](test/LazinessSpec.hs)

## Quick Facts
- Applicative is subclass of Functor
- Alternative is subclass of Applicative
- Monad is a subclass of Applicative
- MonadPlus is a subclass of both Alternative and Monad
- Monoid is a subclass of Semigroup

## Symbol Guide

    $     function application (used to avoid parentheses)
    &     reverse function application (like a Unix pipe)
    .     right-to-left function composition
    ++    list append
    <>    semigroup append
    <$>   infix fmap (like ($) for functors)
    <&>   flipped infix fmap (like (&) for functors)
    <*>   applicative apply
    <$    replace all elements with a value (fmap . const)
    $>    flipped version of (<$)
    <*    like (<*>), but ignore result of second argument
    *>    like (<*>), but ignore result of first argument
    >>=   bind
    =<<   flipped bind
    >=>   left-to-right monadic composition
    <=<   right-to-left monadic composition
    <|>   alternative or
    <-    bind (when used in do block)

## Some possible settings for Visual Studio Code
    {
      "window.zoomLevel": 2,
      "editor.tabSize": 2,
      "editor.detectIndentation": false,
      "files.trimTrailingWhitespace": true
    }

