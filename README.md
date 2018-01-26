# Haskell Fundamentals
Learning project for haskell

## [Think Functionally](http://gitlab.cj.com/jaking/think-functionally-talk)
- Write functions...
    - not methods
    - not classes
    - not procedures
- ...that compose...
    - higher-order functions
    - combinators
    - abstractions should compose
    - can be domain specific
- ...mostly small.
    - prefer lots of little functions
    - build programs out of small pieces
    - rethink “single responsibility”
        - instead of grouping functions by domain as you would in Object Oriented Programming
        - try to make functions more generic so that they can be decoupled from the domain

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

## Data Types
- Bool
    - an enumeration of True and False
- Char
    - a unicode code point between 0 and 1114111 (17 planes with 2^16 code points per plane = 1114112)
- String
    - a list of Char
- Data.Text
    - capabilities of a String, without the performance limitations of a singly linked list
- List
    - represented with brackets [,]
    - a singly linked list
- Tuple
    - represented with parenthesis (,)
    - an algebraic data type whose members are accessed by position but not name
    - the larger the tuple, the less likely there exists language and library support
    - languange guarantees support of at least size 15,
- Unit
    - represented as ()
    - a type with only a single value
    - useful when the return value does not matter
- IO
    - an operation whose behavior is not determined by the program, but rather the environment within which the program is run (for example, the operating system)
- Integer
    - an arbitrary precision integer
- Int
    - a fixed precison integer, 30 bits of precision guaranteed, most likely matches your environment's word size
- Float
    - IEEE 754 single precision floating point
- Double
    - IEEE 754 double precision floating point
- Rational
    - a ratio of Integers

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

