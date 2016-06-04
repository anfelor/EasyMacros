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

import Data.List.NonEmpty as N
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Language.Haskell.Meta.Parse as Haskell

-- | Parse Haskell syntax with a QuasiQuoter
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

-- | Combines Exp, Pat, Type and [Dec]
class Code a
instance Code Exp
instance Code Pat
instance Code Type
instance a ~ Dec => Code [a]

-- | Compile a String into an object in the Q monad of type Code a
newtype Code a => Compiler a = Compiler {compile :: String -> Q a}

-- | A macro. It takes a Compiler and returns a NonEmpty of Code a => Q a.
newtype Code a => Macro a = Macro (Compiler a -> NonEmpty (Q a))

toMacro :: Code a => NonEmpty String -> Macro a
toMacro strs = Macro (\cmp -> fmap (compile cmp) strs)

-- | Convenience function: Apply a Compiler to a Macro, getting results
withCmp :: Code a => Compiler a -> Macro a -> NonEmpty (Q a)
withCmp cmp (Macro m) = m cmp

-- | Convenience function: Apply a QuasiQuoter to a Macro, results are of type ExpQ
withQQ :: QuasiQuoter -> Macro Exp -> NonEmpty ExpQ
withQQ qq (Macro m) = m (Compiler (quoteExp qq))

-- | Transform a nonempty list of ExpQ to a ExpQ containing the same structure
nonEmptyE :: NonEmpty ExpQ -> Q Exp -- contains []
nonEmptyE (e :| es) = [| $e :| $(listE es) |]

-- | Transform a nonempty list of ExpQ to a ExpQ containing a list structure
macroList :: NonEmpty ExpQ -> Q Exp
macroList (e :| es) = [| $e : $(listE es) |]