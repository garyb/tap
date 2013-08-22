tap
===

A programming language.

Very much inspired by Haskell, but strictly evaluated, and the current compile 
target is Javascript, but will probably also produce code for the JVM at some 
point. 

The language is no where near as advanced as Haskell and never will be. It's 
pretty much Haskell98 + multi-parameter type classes + functional dependencies 
+ rank n types, although the typeclass system will differ a little from 
Haskell's.

The language syntax is currently based on s-expressions, but that may change in
the future.

There is a working interpreter, but it only works with code that does not use 
typeclasses, as the compiler transformation for representing typeclasses as 
standard data types has not been completed yet.

Currently the core type inference algorithm is being reworked to handle rank n 
types, as this will be needed for the internal representation of typeclasses as 
standard data types.

There are quite a lot of tests, but many more are still needed no doubt.

GADTs would be nice in the distant future...
