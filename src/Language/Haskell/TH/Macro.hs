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

-- | Parse one line.
type Parser a = String -> Either String a

-- | Compile a list of elements into a Q Exp
type Compiler a = NonEmpty a -> Either String (Q Exp) 

-- | Stores a list of strings. Given a parser and a compiler,
-- the parser maps line by line over the input, and the compiler
-- reduces the results to a Q Exp. 
newtype Block = Block { evaluate :: forall a. Parser a -> Compiler a -> Q Exp }

-- | An error occurring while parsing
data ParseError = ParseError String Int (NonEmpty String)
    deriving (Typeable)

-- | Pretty print as "Parse error: MESSAGE at line LINE: BLOCK"
instance Show ParseError where
    show (ParseError msg linenum exprs) = "Parse error: " ++ msg
      ++ " at line " ++ show linenum ++ ": \n\t"
      ++ intercalate "\n\t" (toList exprs)

-- | default implementation
instance Exception ParseError

-- | An error occurring while compiling
data CompileError = CompileError String (NonEmpty String)

-- | Pretty print as "Compile error: MESSAGE: BLOCK"
instance Show CompileError where
    show (CompileError msg exprs) = "Compile error: "
      ++ msg ++ ":\n\t" ++ intercalate "\n\t" (toList exprs)

-- | default implementation
instance Exception CompileError

-- | Turn a list of strings into a block.
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

-- | Turn a maybe value into an Error, given an error message
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

-- | Left-fold a list of expressions
foldExps :: Q Exp {-(b -> a -> b)-} -> Q Exp {- b -} -> [Exp] -> Q Exp
foldExps _ acc [] = acc
foldExps fn acc (x:xs) = foldExps fn [| $fn $acc $(return x) |] xs

-- | evaluate with arguments flipped.
eval :: Parser a -> Compiler a -> Block -> Q Exp
eval p c b = evaluate b p c

-- | Parse a string as a Haskell expression
haskellExp  :: Parser Exp
haskellExp  = Haskell.parseExp

-- | Parse a string as a Haskell pattern
haskellPat  :: Parser Pat
haskellPat  = Haskell.parsePat