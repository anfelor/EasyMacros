{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Language.Haskell.TH.Macro
-- Copyright   :  (c) Anton Lorenzen 2016
-- License     :  BSD3
-- Maintainer  :  Anton Lorenzen <anfelor@posteo.de>
-- Stability   :  experimental
-- Portability :  portable (template-haskell)
--
-- Basic combinators to build your own macros

module Language.Haskell.TH.Macro where

import Data.List.NonEmpty (NonEmpty(..))
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Language.Haskell.Meta.Parse as Haskell

-- | Parse Haskell syntax with a QuasiQuoter, should be implemented by the haskell compiler
haskell :: QuasiQuoter
haskell = QuasiQuoter
  { quoteExp  = return . fromRight . Haskell.parseExp
  , quotePat  = return . fromRight . Haskell.parsePat
  , quoteType = return . fromRight . Haskell.parseType
  , quoteDec  = return . fromRight . Haskell.parseDecs
  }
  where
    fromRight :: Either String a -> a
    fromRight (Left str) = error str
    fromRight (Right a)  = a

-- | Parse a string as a Haskell expression
haskellExp  :: String -> Q Exp
haskellExp  = quoteExp  haskell

-- | Parse a string as a Haskell pattern
haskellPat  :: String -> Q Pat
haskellPat  = quotePat  haskell

-- | Parse a string as a Haskell type
haskellType :: String -> Q Type 
haskellType = quoteType haskell

-- | Parse a string as a Haskell declaration
haskellDec  :: String -> Q [Dec]
haskellDec  = quoteDec  haskell

-- | A way of compiling a source string into an object in the Q monad.
newtype Compiler a = Compiler { compile :: String -> Q a}

-- | Every {  } literal is compiled to this type.
-- Every expression seperated by ; is stored as a single string.
type Syntax = NonEmpty String

-- | Compiled Syntax.
type Code a = NonEmpty (Q a)

-- | Convenience function: Apply a Compiler to a Macro, getting actual code
withCmp :: Compiler a -> Syntax -> Code a
withCmp cmp m = compile cmp <$> m

-- | Convenience function: Apply a QuasiQuoter to a Macro.
withQQ :: QuasiQuoter -> Syntax -> Code Exp
withQQ qq = withCmp (Compiler (quoteExp qq))

-- | Transform a nonempty list of ExpQ to a ExpQ containing the same structure
nonEmptyE :: Code Exp -> Q Exp -- contains []
nonEmptyE (e :| es) = [| $e :| $(listE es) |]

-- | Transform a nonempty list of ExpQ to a ExpQ containing a list structure
macroList :: Code Exp -> Q Exp
macroList (e :| es) = [| $e : $(listE es) |]