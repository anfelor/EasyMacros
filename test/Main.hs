{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.List.NonEmpty as N
import Language.Haskell.TH.Macro

test :: [Int]
test = $(macroList $ withQQ haskell $ Macro $ \cmp -> compile cmp <$> ("sum [1,2,3]" :| ["1+2"]))
-- withExtension:
-- test = macroList $ withQQ haskell
--     sum [1,2,3]
--     1+2 

test2 = Prelude.map 
     (^2) 
     [1,2,3]

main :: IO ()
main = print test