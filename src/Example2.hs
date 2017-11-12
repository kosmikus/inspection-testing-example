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

mapIK :: (a -> b) -> I a -> K b a
mapIK f (I a) = K (f a)

example :: NP I '[a,b] -> NP (K ()) '[a,b]
example = map_NP (\ (I _) -> K ())

direct :: NP I '[a,b] -> NP (K ()) '[a,b]
direct = \ (I _ :* I _ :* Nil) -> K () :* K () :* Nil

variant :: forall a b . NP I '[a,b] -> NP (K ()) '[a,b]
variant = \ xs -> case (xs :: NP I '[a,b]) of
  I _ :* ys -> case (ys :: NP I '[b]) of
    I _ :* zs -> case (zs :: NP I '[]) of
      Nil -> K () :* K () :* Nil

variant' :: forall a b . NP I '[a,b] -> NP (K ()) '[a,b]
variant' = \ xs -> case (xs :: NP I '[a,b]) of
  I (_ :: a') :* ys -> case (ys :: NP I '[b]) of
    I (_ :: b') :* zs -> case (zs :: NP I '[]) of
      Nil -> (K () :* K () :* Nil :: NP (K ()) '[a',b'])

variant'' :: forall a b . NP I '[a,b] -> NP (K ()) '[a,b]
variant'' = \ xs -> case (xs :: NP I '[a,b]) of
  I (_ :: a') :* ys -> (K () :: K () a') :* case (ys :: NP I '[b]) of
    I (_ :: b') :* zs -> (K () :: K () b') :* case (zs :: NP I '[]) of
      Nil -> Nil

variant''' :: forall a b . NP I '[a,b] -> NP (K ()) '[a,b]
variant''' = \ xs -> case (xs :: NP I '[a,b]) of
  y :* ys -> mapIK (const ()) y :* case (ys :: NP I '[b]) of
    z :* zs -> mapIK (const ()) z :* case (zs :: NP I '[]) of
      Nil -> Nil

variant'''' :: forall a b . NP I '[a,b] -> NP (K ()) '[a,b]
variant'''' = \ xs -> case (xs :: NP I '[a,b]) of
  y :* ys -> case (ys :: NP I '[b]) of
    z :* zs -> case (zs :: NP I '[]) of
      Nil -> mapIK (const ()) y :* mapIK (const ()) z :* Nil

-- inspect $ 'example === 'direct    -- fails
-- inspect $ 'example ==- 'variant   -- fails
-- inspect $ 'example ==- 'variant'  -- fails
inspect $ 'example === 'variant''   -- succeeds
inspect $ 'example ==- 'variant'''  -- succeeds
-- inspect $ 'example ==- 'variant'''' -- fails

