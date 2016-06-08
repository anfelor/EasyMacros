# Proposal: EasyMacros (work in progress!)
[![Build Status](https://travis-ci.org/anfelor/EasyMacros.svg?branch=master)](https://travis-ci.org/anfelor/EasyMacros)

This is the library implementation of the EasyMacros proposal to the Haskell
programming language. It is based on the idea, that
```haskell
{x <- getLine; putStrLn x}
```
should be syntactic sugar for
```haskell
blockify ("x <- getLine" :| ["putStrLn x"]) :: Block
```

This means that the code could easily be transformed by TemplateHaskell. A macro would then be [defined as](src/Language/Haskell/TH/StandardMacros.hs#L33)
```haskell
macro do
do :: Block -> Q Exp
```
and a call would be desugered into:
```haskell
main = $(do $ blockify 
    ( "x <- getLine" :| 
    [ "putStrLn x"
    ]))
```

The last example already works: take a look at [the tests](test/Main.hs#13).

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

Also (some) new additions to the language could be implemented as Haskell Libraries, for example ApplicativeDo, RecursiveDo,  MultiWayIf (as the [cond macro](src/Language/Haskell/TH/StandardMacros.hs#72)) and RebindableSyntax.

In the case of RebindableSyntax one would probably want to chain the statements with (mkName ">>=") instead of the GHC.Base.>>= used in the example implementation in this project. Then one would simply have to type:
```haskell
import Prelude hiding ((do))
import Language.Haskell.RebindableSyntax ((do))
```

## Open Questions
### NonEmpty or List
I've chosen to use NonEmpty for the source code. This reflects the fact, that a { } literal may not be empty.
However a real list ([ ]) would be more convenient.

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
-- | Parse one line.
type Parser a = String -> Either String a

-- | Compile a list of elements into a Q Exp
type Compiler a = NonEmpty a -> Either String (Q Exp) 

-- | Stores a list of strings. Given a parser and a compiler,
-- the parser maps line by line over the input, and the compiler
-- reduces the results to a Q Exp. 
newtype Block = Block { evaluate :: forall a. Parser a -> Compiler a -> Q Exp }

haskellExp  :: Parser Exp
haskellPat  :: Parser Pat
```
That means that the implementation of a Haskell-Parser as part of GHC is absolutely necessary for the proposal!
(This is why this library has haskell-src-meta as a dependency)

### The "macro" keyword 
The "macro" keyword should be quite similar to the "infix" keyword.
A function annotated with it must return a Q Exp (Option: Q [Dec] might be possible also...).

Option: If every argument would be of type Q Exp or Block,
it could be considered, that passing arguments can happen without [|  |]

If there are parenthesis around the name of a macro, the macro is turned into a usual function.
```haskell
import Prelude ((do), (>>=))
map (do) [{some; content }, {;}] :: [Q Exp] -- *not* desugered into $(map ..)
```
