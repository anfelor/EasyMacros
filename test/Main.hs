{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Language.Haskell.TH.Macro
import Language.Haskell.TH.StandardMacros

import System.Directory

test1 :: IO ()
test1 = $(do_ 
    ( "dirs <- getDirectoryContents \"/\"" :| 
    [ "let prefix = '/'"
    , "let suffix = prefix"
    , "let stdout str = putStrLn str"
    , "mapM_ (stdout . (++[suffix]) . (prefix:)) dirs"
    ]))

test3 :: [Int]
test3 = $( apply
    ( "[1..(10*365)]" :|
    [ "map (replicate 3)"
    , "concat"
    , "take 10"
    ]))

test4 :: String
test4 = intercalate "\n" $(list
    ( "\"Hello World!\"" :|
    [ "\"How are you?\"" 
    , "\"Yours, Anton\""]))

test5 :: Maybe [Int]
test5 = $(conc 
    ( "Just [3]" :|
    [ "Just [4]" 
    , "Nothing" ]))
    
main :: IO ()
main = do
    putStrLn " -- Tests:"
    test1
    print test3
    putStrLn test4
    print test5
