{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

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

import Control.Exception (Exception, throw)
import Data.List
import Data.List.NonEmpty (NonEmpty(..), toList, fromList)
import Data.Typeable (Typeable)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Language.Haskell.Meta.Parse as Haskell

type Parser   a = String     -> Either String a
type Compiler a = NonEmpty a -> Either String (Q Exp) 

newtype Block = Block { evaluate :: forall a. Parser a -> Compiler a -> Q Exp }


data ParseError = ParseError String Int (NonEmpty String)
    deriving (Typeable)

instance Show ParseError where
    show (ParseError msg linenum exprs) = "Parse error: " ++ msg
      ++ " at line " ++ show linenum ++ ": \n\t"
      ++ intercalate "\n\t" (toList exprs)

instance Exception ParseError


data CompileError = CompileError String (NonEmpty String)

instance Show CompileError where
    show (CompileError msg exprs) = "Compile error: "
      ++ msg ++ ":\n\t" ++ intercalate "\n\t" (toList exprs)

instance Exception CompileError


blockify :: NonEmpty String -> Block
blockify exprs = Block (\p c -> compile c $ parse p) 
  where
    parse :: Parser a -> NonEmpty a
    parse p = fromList $ check <$> zip (toList $ p <$> exprs) [1..]
      where
        check :: (Either String a, Int) -> a
        check (parsed, linenum)
          | (Right a)  <- parsed = a
          | (Left msg) <- parsed = throw (ParseError msg linenum exprs)
    
    compile :: Compiler a -> NonEmpty a -> Q Exp
    compile cmp parsed = check (cmp parsed)
      where
        check :: Either String (Q Exp) -> Q Exp
        check compiled
          | (Right a)  <- compiled = a
          | (Left msg) <- compiled = throw (CompileError msg exprs)

maybeToError :: String -> Maybe a -> Either String a
maybeToError _ (Just x) = Right x
maybeToError err Nothing = Left err 

-- | Transform a nonempty list of ExpQ to a ExpQ containing the same structure
nonEmptyE :: NonEmpty (Q Exp) -> Q Exp -- contains []
nonEmptyE (e :| es) = [| $e :| $(listE es) |]
-- 
-- | Transform a nonempty list of ExpQ to a ExpQ containing a list structure
macroList :: NonEmpty (Q Exp) -> Q Exp
macroList (e :| es) = [| $e : $(listE es) |]

-- | Classic left fold over Syntax and with Q Exp.
-- All strings in the block are evaluated as a haskell expression! 
foldC :: Q Exp {-(b -> a -> b)-} -> Q Exp {- b -} -> Block -> Q Exp
foldC f a1 = eval haskellExp (Right . foldExps f a1 . toList)

-- | Classic foldl1, see above
foldC1 :: Q Exp {-(b -> a -> b)-} -> Block -> Q Exp
foldC1 fn = eval haskellExp (\e -> 
                  let (a : as) = toList e in Right $ foldExps fn (return a) as)

foldExps :: Q Exp {-(b -> a -> b)-} -> Q Exp {- b -} -> [Exp] -> Q Exp
foldExps _ acc [] = acc
foldExps fn acc (x:xs) = foldExps fn [| $fn $acc $(return x) |] xs

eval :: Parser a -> Compiler a -> Block -> Q Exp
eval p c b = evaluate b p c

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
haskellExp  :: String -> Either String Exp
haskellExp  = Haskell.parseExp

-- | Parse a string as a Haskell pattern
haskellPat  :: String -> Either String Pat
haskellPat  = Haskell.parsePat