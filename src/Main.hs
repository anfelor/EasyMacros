{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import TH
import RegExp as RegExp hiding (apply)

import Language.Haskell.TH
import Data.Semigroup

main :: IO ()
main = do
    putStrLn hugeString
    print longCalculation
    print (isEmailAddr2 "me@my-company.com")

echo :: IO ()
echo = $( do_
    [ varP (mkName "x") `bindS` [| getLine |]
    , noBindS [| putStrLn $(varE (mkName "x")) |]
    ] )

isEmailAddr2 :: String -> Bool
isEmailAddr2 = RegExp.match $( combine
    [ [regex|([a-z]|[0-9])*|]
    , [regex|@|]
    , [regex|([a-z]|[0-9]|-)*|]
    , [regex|.com|]
    ] )

hugeString :: String
hugeString = $( conc
    [ [| "Hello World! " |]
    , [| "How are you? " |]
    ] )
    
longCalculation :: [Int]
longCalculation = $( apply 
    [ [| [1..(10*365)] |]
    , [| map (replicate 3) |]
    , [| concat |]
    ] )
