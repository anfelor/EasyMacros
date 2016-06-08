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
    , cond
    ) where

import Control.Applicative
import Data.Maybe
import Data.Semigroup
import Data.List
import Data.List.NonEmpty (NonEmpty(..), fromList, toList)
import Language.Haskell.TH

import Language.Haskell.TH.Macro
import Debug.Trace

-- | The do notation of Haskell implemented as a macro.
-- Expressions are chained with GHC.Base.>>=, so it is not possible to rebind syntax.
-- This is only an example, it does not use ApplicativeDo for instance!
do_ :: Syntax -> Q Exp
do_ = doS . withCmp (Compiler cmp)
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
   
    cmp :: String -> StmtQ
    cmp str = trace str $ fromMaybe nobindS $ binding <|> letExpr
      where
        binding, letExpr :: Maybe (Q Stmt)
        binding = divideAtSymbol "<-" str >>= \(pat, ex) ->
                        Just $ liftA2 BindS (haskellPat pat) (haskellExp ex)
                        
        letExpr = divideAtSymbol "=" str >>= \(pat, ex) -> Just $ LetS . (:[]) <$> var (drop 4 pat, ex)
          where
            var :: (String, String) -> Q Dec 
            var (pat, ex)
              | ' ' `elem` trim pat = FunD (mkName (takeWhile (/=' ') pat)) . (:[]) <$> liftA3 Clause 
                      (mapM haskellPat (split " " (trim (dropWhile (/=' ') pat))))
                      (fmap NormalB (haskellExp ex))
                      (return [])
              | otherwise = ValD <$> haskellPat pat <*> (NormalB <$> haskellExp ex) <*> return []
                    
        nobindS :: Q Stmt
        nobindS = NoBindS <$> haskellExp str

-- | MultiWayIf as a macro. The syntax is
-- (Condition :: Bool) -> Expression
-- At least one condition needs to match and all expressions need to evaluate to the same type.
cond :: Syntax -> Q Exp
cond = condS . withCmp (Compiler cmp)
  where
    condS :: Code (Exp, Exp) -> Q Exp
    condS (x :| []) = x >>= \(p, ex) -> [| if $(return p) then $(return ex) else error "Nothing could match!" |]
    condS (x :| xs) = x >>= \(p, ex) -> [| if $(return p) then $(return ex) else $(condS (fromList xs)) |]
    
    cmp :: String -> Q (Exp, Exp)
    cmp s = let (f', s') = fromJust $ divideAtSymbol "->" s
            in (,) <$> haskellExp f' <*> haskellExp s'


-- | Classic left fold over Syntax and with Q Exp.
-- All strings in Syntax are evaluated as a haskell expression! 
foldC :: Q Exp {-(b -> a -> b)-} -> Q Exp {- b -} -> Syntax -> Q Exp
foldC f a1 m = go f a1 $ map haskellExp (toList m)
  where
    go :: Q Exp {-(b -> a -> b)-} -> Q Exp {- b -} -> [Q Exp] -> Q Exp
    go _ acc [] = acc
    go fn acc (x:xs) = go fn [| $fn $acc $x |] xs

-- | Classic foldl1, see above
foldC1 :: Q Exp {-(b -> a -> b)-} -> Syntax -> Q Exp
foldC1 fn (a :| as) = foldC fn  [| $(haskellExp a) |] (fromList as)

-- | Apply functions to arguments.
-- Evaluates to: thr-line (snd-line (fst-line))
apply :: Syntax -> Q Exp
apply = foldC1 [| flip ($) |]

-- | Concatenate the arguments with (<>)
-- All the strings in Syntax are evaluated as Haskell code,
-- which needs to be an instance of Semigroup.
conc :: Syntax -> Q Exp
conc = foldC1 [| (<>) |]

-- | Turn all the Expressions into a list.
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
