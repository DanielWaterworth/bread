{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Main where

import Data.Foldable (Foldable)
import Data.Traversable

import Control.Applicative (Applicative(..))
import Control.Monad (ap)

import Prelude.Extras

import Bound

data QBF a =
  Literal a |
  Const Bool |
  And [QBF a] |
  Or [QBF a] |
  Not (QBF a) |
  Exists (Scope () QBF a) |
  Forall (Scope () QBF a) |
  Let (QBF a) (Scope () QBF a)
    deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance Eq1 QBF
instance Ord1 QBF
instance Show1 QBF
instance Read1 QBF

instance Applicative QBF where
  pure = return
  (<*>) = ap

instance Monad QBF where
  return = Literal

  Literal v >>= f = f v
  Const b >>= f = Const b
  And xs >>= f = And $ map (\x -> x >>= f) xs
  Or xs >>= f = Or $ map (\x -> x >>= f) xs
  Not x >>= f = Not $ x >>= f
  Exists x >>= f = Exists $ x >>>= f
  Forall x >>= f = Forall $ x >>>= f
  Let x y >>= f = Let (x >>= f) (y >>>= f)

constantProp :: Eq a => QBF a -> QBF a
constantProp (Literal a) = Literal a
constantProp (Const b) = Const b
constantProp (And xs) =
  let
    xs' = filter (/= Const True) $ map constantProp xs
  in
    if Const False `elem` xs' then
      Const False
    else
      case xs' of
        [] -> Const True
        [x] -> x
        _ -> And xs'
constantProp (Or xs) =
  let
    xs' = filter (/= Const False) $ map constantProp xs
  in
    if Const True `elem` xs' then
      Const True
    else
      case xs' of
        [] -> Const False
        [x] -> x
        _ -> Or xs'
constantProp (Not x) =
  case constantProp x of
    Const b -> Const (not b)
    x' -> Not x'
constantProp (Exists x) =
  case constantProp $ fromScope x of
    Const b -> Const b
    x' -> Exists $ toScope $ x'
constantProp (Forall x) =
  case constantProp $ fromScope x of
    Const b -> Const b
    x' -> Forall $ toScope $ x'
constantProp (Let x y) =
  case constantProp x of
    Const b ->
      constantProp (instantiate1 (Const b) y)
    x' ->
      case constantProp $ fromScope y of
        Const b -> Const b
        y' ->
          Let x' $ toScope y'

distributeNots :: QBF a -> QBF a
distributeNots (Not (Not x)) = distributeNots x
distributeNots (Not (Exists x)) =
  Forall (toScope (distributeNots (Not (fromScope x))))
distributeNots (Not (Forall x)) =
  Exists (toScope (distributeNots (Not (fromScope x))))
distributeNots (Not (And xs)) =
  Or $ map (distributeNots . Not) xs
distributeNots (Not (Or xs)) =
  And $ map (distributeNots . Not) xs
distributeNots (Let x y) =
  Let (distributeNots x) (toScope (distributeNots (fromScope y)))
distributeNots (And xs) =
  And $ map distributeNots xs
distributeNots (Or xs) =
  Or $ map distributeNots xs
distributeNots (Exists x) =
  Exists $ toScope $ distributeNots $ fromScope x
distributeNots (Forall x) =
  Forall $ toScope $ distributeNots $ fromScope x
distributeNots x = x

collapseOr' :: QBF a -> QBF a
collapseOr' x =
  case collapseOr x of
    [y] -> y
    ys -> Or ys

collapseOr :: QBF a -> [QBF a]
collapseOr (Or xs) = concatMap collapseOr xs
collapseOr (And xs) = [And (map collapseOr' xs)]
collapseOr (Not x) = [Not (collapseOr' x)]
collapseOr (Forall x) = [Forall (toScope (collapseOr' (fromScope x)))]
collapseOr (Exists x) = [Exists (toScope (collapseOr' (fromScope x)))]
collapseOr (Let x y) = [Let (collapseOr' x) (toScope (collapseOr' (fromScope y)))]
collapseOr x = [x]

collapseAnd' :: QBF a -> QBF a
collapseAnd' x =
  case collapseAnd x of
    [y] -> y
    ys -> And ys

collapseAnd :: QBF a -> [QBF a]
collapseAnd (And xs) = concatMap collapseAnd xs
collapseAnd (Or xs) = [Or (map collapseAnd' xs)]
collapseAnd (Not x) = [Not (collapseAnd' x)]
collapseAnd (Forall x) = [Forall (toScope (collapseAnd' (fromScope x)))]
collapseAnd (Exists x) = [Exists (toScope (collapseAnd' (fromScope x)))]
collapseAnd (Let x y) = [Let (collapseAnd' x) (toScope (collapseAnd' (fromScope y)))]
collapseAnd x = [x]

--solve :: Hashable k => QBF k -> IO (Maybe (HashMap k Bool))

main = print $ constantProp $ And [Exists (toScope (Exists (toScope (Literal (F (B ())))))), Literal 4]
