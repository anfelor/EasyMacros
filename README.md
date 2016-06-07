# Proposal: EasyMacros (work in progress!)
This is the library implementation of the EasyMacros proposal to the Haskell
programming language. It is based on the idea, that
```haskell
{x <- getLine; putStrLn x}
```
should be syntactic sugar for
```haskell
("x <- getLine" :| ["putStrLn x"]) :: Syntax
```

This means that the code could easily be transformed by TemplateHaskell. A macro would then be [defined as](file://src/Language/Haskell/TH/StandardMacros.hs)
```haskell
macro do
do :: Syntax -> Q Exp
```
and a call would be desugered into:
```haskell
main = $(do 
    ( "x <- getLine" :| 
    [ "putStrLn x"
    ]))
```

The last example already works: take a look at [the tests](file://test/Main.hs).

## Benefits
It would be much easier to use TemplateHaskell functions. This might allow for some nice syntax additions like
```haskell
test3 :: [Int]
test3 = apply
    [1..(10*365)]
    map (replicate 3)
    concat
    take 10
```

Also (some) new additions to the language could be implemented as Haskell Libraries, for example ApplicativeDo, RecursiveDo,  MultiWayIf (in parts) and RebindableSyntax.

In the case of RebindableSyntax one would probably want to chain the statements with (mkName ">>=") instead of the GHC.Base.>>= used in the example implementation in this project. Then one would simply have to type:
```haskell
import Prelude hiding ((do))
import Language.Haskell.RebindableSyntax ((do))
```

## Open Questions
### NonEmpty or List
I've chosen to use NonEmpty for the source code. This reflects the fact, that a { } literal may not be empty.
However [ ] would be more convenient.

### Desugering into { }
While the implementation of transforming { } into a list of strings should be trivial, that is not the case for the desugering of bracketless syntax:
```haskell
main = do 
  x <- getLine
  putStrLn x
  
main = Prelude.map
  (^2) 
  [1,2,3]
``` 
Map is not a macro, nonetheless the arguments above should not be parsed as if it were one.

## Implementation

The code is written around the definitions:
```haskell
newtype Compiler a = Compiler { compile :: String -> Q a}
type Syntax = NonEmpty String
type Code a = NonEmpty (Q a)

withQQ :: QuasiQuoter -> Syntax -> Code Exp
```
That means that the implementation of a Haskell QuasiQuoter is absolutely necessary for the proposal!
(This is why this library has haskell-src-meta as a dependency)

### The "macro" keyword 
The "macro" keyword is quite similar to the "infix" keyword.
A function annotated with it must return a Q Exp (Option: Q [Dec] might be possible also...).

Option: If every argument would be of type Q Exp or Syntax, 
it could be considered, that passing arguments can happen without 

If there are parenthesis around the name of a macro, the macro is turned into a usual function.
```haskell
import Prelude ((do), (>>=))
map (do) [{some; content }, {;}] :: [Q Exp] -- *not* desugered into $(map ..)
```
