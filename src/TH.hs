{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module TH where

import Prelude hiding (exp)
import Data.List.NonEmpty
import Data.Semigroup
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift, lift, Q(Q))

do_ :: NonEmpty (Q Stmt) -> Q Exp
do_ (x :| []) = x >>= \case
    BindS _ _ -> fail "The last statement in a 'do' block must be an expression"
    NoBindS e -> return e
do_ (x :| xs) = x >>= \case
    BindS pat exp -> [| $(return exp) >>= \ $(return pat) -> $(do_ (fromList xs)) |]
    NoBindS   exp -> [| $(return exp) >> $(do_ (fromList xs)) |]

combine :: (Lift m, Semigroup m) => NonEmpty m -> Q Exp
combine = with (<>)

with :: Lift a => (a -> a -> a) -> NonEmpty a -> Q Exp
with _ (a :| []) = lift a
with fn (a :| b : xs) = with fn ((a `fn` b) :| xs)


conc = fold [| (++) |]
apply = fold [|flip ($)|]

fold :: ExpQ -- (a -> b -> c)
  -> NonEmpty (Q Exp) -> Q Exp
fold _ (a :| []) = a
fold fn (a :| b : xs) = fold fn (([| fn $a $b |]) :| xs)

-- unsafeWith :: (a -> b -> c) -> NonEmpty (Q Exp) -> Q Exp
-- unsafeWith fn = with (\ a b -> appE (appE [| fn |] a) b)

instance Semigroup (Q Exp) where
    q1 <> q2 = [| $q1 <> $q2 |]
    
instance Lift (Q Exp) where
    lift q = [| $q |]