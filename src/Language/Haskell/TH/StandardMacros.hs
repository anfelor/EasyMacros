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
do_ :: Block -> Q Exp
do_ = eval cmp doS
  where
    doS :: Compiler Stmt
    doS (x :| []) = case x of
      BindS _ _ -> Left "The last statement in a 'do' block must be an expression"
      LetS _    -> Left "The last statement in a 'do' block must be an expression"
      NoBindS e -> Right $ return e
    doS (x :| xs) = let xs' = doS (fromList xs) in case xs' of
      Left str -> Left str
      Right xs'' -> Right $ case x of
        BindS pat ex -> [| $(return ex) >>= \ $(return pat) -> $(xs'') |]
        LetS     dec -> letE (return <$> dec) xs''
        NoBindS   ex -> [| $(return ex) >> $xs'' |] 
   
    cmp :: Parser Stmt
    cmp str = trace str $ fromMaybe nobindS $ binding <|> letExpr
      where
        binding, letExpr :: Maybe (Either String Stmt)
        binding = divideAtSymbol "<-" str >>= \(pat, ex) ->
                        Just $ liftA2 BindS (haskellPat pat) (haskellExp ex)
                         
        letExpr = divideAtSymbol "=" str >>= \(pat, ex) -> 
                        Just $ LetS . (:[]) <$> var (drop 4 pat, ex)
          where
            var :: (String, String) -> Either String Dec 
            var (pat, ex)
              | ' ' `elem` trim pat = FunD (mkName (takeWhile (/=' ') pat)) . (:[]) <$> liftA3 Clause 
                      (mapM haskellPat (split " " (trim (dropWhile (/=' ') pat))))
                      (fmap NormalB (haskellExp ex))
                      (return [])
              | otherwise = ValD <$> haskellPat pat <*> (NormalB <$> haskellExp ex) <*> return []
                    
        nobindS :: Either String Stmt
        nobindS = NoBindS <$> haskellExp str

-- | MultiWayIf as a macro. The syntax is
-- (Condition :: Bool) -> Expression
-- At least one condition needs to match and all expressions need to evaluate to the same type.
cond :: Block -> Q Exp
cond = eval cmp (Right . condS)
  where
    condS :: NonEmpty (Exp, Exp) -> Q Exp
    condS ((p, ex) :| []) = [| if $(return p) then $(return ex) else error "Nothing could match!" |]
    condS ((p, ex) :| xs) = [| if $(return p) then $(return ex) else $(condS (fromList xs)) |]
    
    cmp :: Parser (Exp, Exp)
    cmp s = maybeToError "No '->' included!" (divideAtSymbol "->" s) >>= 
              \(f', s') -> (,) <$> haskellExp f' <*> haskellExp s'


-- | Apply functions to arguments.
-- Evaluates to: thr-line (snd-line (fst-line))
apply :: Block -> Q Exp
apply = foldC1 [| flip ($) |]

-- | Concatenate the arguments with (<>)
-- All the strings in Syntax are evaluated as Haskell code,
-- which needs to be an instance of Semigroup.
conc :: Block -> Q Exp
conc = foldC1 [| (<>) |]

-- | Turn all the Expressions into a list.
list :: Block -> Q Exp
list = eval haskellExp (Right . macroList . fmap return)

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
