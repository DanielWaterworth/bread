{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Main where

import Data.Foldable (Foldable)
import Data.Traversable

import Control.Applicative (Applicative(..))
import Control.Monad (ap)

import Prelude.Extras

import Bound

data QBF a =
  Var a |
  Const Bool |
  And [QBF a] |
  Or [QBF a] |
  Not (QBF a) |
  Exists (Scope () QBF a) |
  Forall (Scope () QBF a)
    deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance Eq1 QBF
instance Ord1 QBF
instance Show1 QBF
instance Read1 QBF

instance Applicative QBF where
  pure = return
  (<*>) = ap

instance Monad QBF where
  return = Var

  Var v >>= f = f v
  Const b >>= f = Const b
  And xs >>= f = And $ map (\x -> x >>= f) xs
  Or xs >>= f = Or $ map (\x -> x >>= f) xs
  Not x >>= f = Not $ x >>= f
  Exists x >>= f = Exists $ x >>>= f
  Forall x >>= f = Forall $ x >>>= f

constantProp :: Eq a => QBF a -> QBF a
constantProp (Var a) = Var a
constantProp (Const b) = Const b
constantProp (And []) = Const True
constantProp (And xs) =
  let
    xs' = map constantProp xs
  in
    if Const False `elem` xs' then
      Const False
    else
      And xs'
constantProp (Or []) = Const False
constantProp (Or xs) =
  let
    xs' = map constantProp xs
  in
    if Const True `elem` xs' then
      Const True
    else
      Or xs'
constantProp (Not x) =
  let
    x' = constantProp x
  in
    case x' of
      Const b -> Const (not b)
      _ -> Not x'
constantProp (Exists x) =
  let
    x' = constantProp $ fromScope x
  in
    case x' of
      Const b -> Const b
      _ -> Exists $ toScope $ x'
constantProp (Forall x) =
  let
    x' = constantProp $ fromScope x
  in
    case x' of
      Const b -> Const b
      _ -> Forall $ toScope $ x'

--solve :: QBF Int -> IO (Maybe (IntMap Bool))

main = undefined
