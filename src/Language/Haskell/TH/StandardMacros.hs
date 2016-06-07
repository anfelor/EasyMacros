{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Language.Haskell.TH.StandardMacros
-- Copyright   :  (c) Anton Lorenzen 2016
-- License     :  BSD3
-- Maintainer  :  Anton Lorenzen <anfelor@posteo.de>
-- Stability   :  experimental
-- Portability :  portable (template-haskell)
--
-- Basic macros in the syntax

module Language.Haskell.TH.StandardMacros
    ( do_
    , foldC
    , foldC1
    , apply
    , conc
    , list
    ) where

import Control.Applicative
import Data.Maybe
import Data.Semigroup
import Data.List
import Data.List.NonEmpty (NonEmpty(..), fromList, toList)
import qualified Data.List.NonEmpty as NonEmpty
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Language.Haskell.Meta.Parse as Haskell

import Language.Haskell.TH.Macro
import Debug.Trace

-- macro do_
do_ :: Syntax -> Q Exp
do_ = doS . withCmp compileStmt
  where
    doS :: Code Stmt -> Q Exp
    doS (x :| []) = x >>= \x' -> case x' of
      BindS _ _ -> fail "The last statement in a 'do' block must be an expression"
      LetS _    -> fail "The last statement in a 'do' block must be an expression"
      NoBindS e -> return e
    doS (x :| xs) = x >>= \x' -> case x' of
      BindS pat ex -> [| $(return ex) >>= \ $(return pat) -> $(doS (fromList xs)) |]
      LetS     dec -> letE (return <$> dec) (doS (fromList xs))
      NoBindS   ex -> [| $(return ex) >> $(doS (fromList xs)) |] 

    compileStmt :: Compiler Stmt
    compileStmt = Compiler cmp
      where    
        cmp :: String -> StmtQ
        cmp str = trace str $ fromMaybe nobindS $ binding <|> letExpr
          where
            binding, letExpr :: Maybe (Q Stmt)
            binding = divideAtSymbol "<-" str >>= \(pat, ex) ->
                            Just $ BindS <$> (haskellPat pat) <*> (haskellExp ex)
            letExpr = divideAtSymbol "=" str >>= \(pat, ex) -> Just $ LetS . (:[]) <$> var (drop 4 pat, ex)
              where
                var :: (String, String) -> Q Dec 
                var (pat, ex)
                  | ' ' `elem` trim pat = FunD (mkName (takeWhile (/=' ') pat)) <$> (:[]) <$> (Clause 
                          <$> mapM haskellPat (split " " (trim (dropWhile (/=' ') pat)))
                          <*> fmap NormalB (haskellExp ex)
                          <*> return [])
                  | otherwise = ValD <$> haskellPat pat <*> (NormalB <$> haskellExp ex) <*> return []
                        
            nobindS :: Q Stmt
            nobindS = NoBindS <$> haskellExp str

-- macro foldC
foldC :: Q Exp {-(b -> a -> b)-} -> Q Exp {- b -} -> Syntax -> Q Exp
foldC fn a1 m = go fn a1 $ map haskellExp (toList m)
  where
    go :: Q Exp {-(b -> a -> b)-} -> Q Exp {- b -} -> [Q Exp] -> Q Exp
    go fn a1 [] = a1
    go fn a1 (x:xs) = go fn [| $fn $a1 $x |] xs

-- macro foldC1
foldC1 :: Q Exp {-(b -> a -> b)-} -> Syntax -> Q Exp
foldC1 fn (a :| as) = foldC fn  [| $(haskellExp a) |] (fromList as)

-- macro apply
apply :: Syntax -> Q Exp
apply = (foldC1) [| flip ($) |]

-- macro conc
conc :: Syntax -> Q Exp
conc = (foldC1) [| (<>) |]

-- macro list
list :: Syntax -> Q Exp
list = macroList . withQQ haskell

-- | Split a list into two parts at the first occurrence of a symbol, not containing the symbol.
divideAtSymbol :: Eq a => [a] -> [a] -> Maybe ([a], [a])
divideAtSymbol sym xs = (,) <$> f <*> s
  where
    f = (\l -> take ((length l) - (length sym)) l) <$> find (isSuffixOf sym) (inits xs)
    s = drop (length sym) <$> find (isPrefixOf sym) (tails xs)

-- | Split a list into many parts at the occurences of a symbol, not containing the symbol.
split :: Eq a => [a] -> [a] -> [[a]]
split sym xs = let strs = map (drop (length sym)) $ filter (isPrefixOf sym) (tails xs)
                   strl = length xs : map length strs ++ [0]
               in zipWith take (zipWith (-) strl (drop 1 strl)) (xs:strs)

-- | Remove whitespace from both ends of the string.
trim :: String -> String
trim = reverse . dropWhile (==' ') . reverse . dropWhile (==' ')
