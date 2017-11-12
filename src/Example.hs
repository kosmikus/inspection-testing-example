{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-idinfo -dsuppress-module-prefixes #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
module Main where

import Test.Inspection

main :: IO ()
main = return ()

data NP :: (k -> *) -> [k] -> * where
  Nil  :: NP f '[]
  (:*) :: f x -> !(NP f xs) -> NP f (x : xs)

infixr 5 :*

class SListI (xs :: [k]) where
  cataSList ::
    r '[]
    -> (forall y ys . SListI ys => r ys -> r (y ': ys))
    -> r xs

instance SListI '[] where
  cataSList nil _cons = nil
  {-# INLINE cataSList #-}

instance SListI xs => SListI (x ': xs) where
  cataSList nil cons =
    cons (cataSList nil cons)
  {-# INLINE cataSList #-}

newtype (f -.-> g) x = Fn { apFn :: f x -> g x }

map_NP ::
  SListI xs => (forall x . f x -> g x) -> NP f xs -> NP g xs
map_NP f =
  apFn $ cataSList
    (Fn (\ Nil -> Nil))
    (\ (Fn rec) -> Fn (\ (x :* xs) -> f x :* rec xs))
{-# INLINE map_NP #-}

newtype I a = I a
newtype K a b = K a

example :: NP I '[a] -> NP (K ()) '[a]
example = map_NP (\ (I _) -> K ())

direct :: NP I '[a] -> NP (K ()) '[a]
direct = \ (I _ :* Nil) -> K () :* Nil

variant :: forall a . NP I '[a] -> NP (K ()) '[a]
variant = \ xs -> case (xs :: NP I '[a]) of
  I _ :* ys -> case (ys :: NP I '[]) of
    Nil -> K () :* Nil

variant' :: forall a . NP I '[a] -> NP (K ()) '[a]
variant' = \ xs -> case (xs :: NP I '[a]) of
  I (_ :: b) :* ys -> case (ys :: NP I '[]) of
    Nil -> (K () :* Nil :: NP (K ()) '[b])

-- inspect $ 'example === 'direct   -- fails
-- inspect $ 'example === 'variant  -- fails
inspect $ 'example ==- 'variant  -- succeeds
inspect $ 'example === 'variant' -- succeeds

-- Core code:
--
-- example :: forall a. NP I '[a] -> NP (K ()) '[a]
-- example
--   = \ (@ a) (ds :: NP I '[a]) ->
--       case ds of { :* @ x @ xs cobox x1 xs1 ->
--       (direct2 @ x)
--       `cast` ((NP
--                  <*>_N <K ()>_R ((':) <*>_N (Nth:1 (Sym cobox)) <'[]>_N)_N)_R
--               :: (NP (K ()) '[x] :: *) ~R# (NP (K ()) '[a] :: *))
--       }
--
-- variant :: forall a. NP I '[a] -> NP (K ()) '[a]
-- variant
--   = \ (@ a) (xs :: NP I '[a]) ->
--         case xs of { :* @ x @ xs1 cobox ds ys -> direct2 @ a }
--
-- variant' :: forall a. NP I '[a] -> NP (K ()) '[a]
-- variant' = example
--
--
-- direct :: forall a. NP I '[a] -> NP (K ()) '[a]
-- direct
--   = \ (@ a) (ds :: NP I '[a]) ->
--       case ds of { :* @ x @ xs cobox ds1 ds2 ->
--       case ds2 of {
--         Nil cobox1 -> direct2 @ a;
--         :* @ ipv @ ipv1 ipv2 ipv3 ipv4 -> direct1 @ a
--       }
--       }
--
-- direct1 :: forall a. NP (K ()) '[a]
-- direct1 = \ (@ a) -> patError @ 'LiftedRep @ (NP (K ()) '[a]) lvl1
--
-- direct2 :: forall x. NP (K ()) '[x]
-- direct2
--   = \ (@ x) ->
--       :*
--         @ *
--         @ (K ())
--         @ '[x]
--         @ x
--         @ '[]
--         @~ (<'[x]>_N :: ('[x] :: [*]) ~# ('[x] :: [*]))
--         (()
--          `cast` (Sym (N:K[0] <*>_N <()>_R <x>_P)
--                  :: (() :: *) ~R# (K () x :: *)))
--         direct3
--
-- direct3 :: NP (K ()) '[]
-- direct3
--   = Nil
--         @ * @ (K ()) @ '[] @~ (<'[]>_N :: ('[] :: [*]) ~# ('[] :: [*]))
--
