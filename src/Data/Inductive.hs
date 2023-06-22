{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Data.Inductive where

import GHC.TypeNats
import GHC.Show
import Data.Kind
import Control.Applicative

type Some :: (a -> Type) -> Type
data Some f where
  MkSome :: f a -> Some f

type Index :: [a] -> a -> Type
data Index xs x where
  Z :: Index (x : xs) x
  S :: Index xs x -> Index (y : xs) x

-- There can be no index without any elements.
noElements :: Index '[] x -> a
noElements i = case i of {}

-- TODO: rewrite rule to simplify this to unsafeCoerce
extendIndexBase :: forall ys xs x. Index xs x -> Index (xs ++ ys) x
extendIndexBase Z = Z
extendIndexBase (S x) = S (extendIndexBase @ys x)

indexToInt :: Index xs x -> Int
indexToInt Z = 0
indexToInt (S n) = 1 + indexToInt n

instance Show (Index xs x) where
  showsPrec p i =
    showParen (p  > appPrec) $
      showString "index @" . showsPrec (appPrec+1) (indexToInt i)

type GetIndex :: forall {a}. Natural -> [a] -> a -> Constraint
class GetIndex i xs x where
  index :: Index xs x

instance {-# OVERLAPPING #-} (x ~ y) => GetIndex 0 (y : xs) x where
  index = Z

instance GetIndex (n - 1) xs x => GetIndex n (y : xs) x where
  index = S (index @(n - 1))

type (++) :: [a] -> [a] -> [a]
type family xs ++ ys where
  '[] ++ ys = ys
  (x : xs) ++ ys = x : (xs ++ ys)

assocListAppend :: forall xs ys zs f r. HList f xs -> (((xs ++ ys) ++ zs) ~ (xs ++ (ys ++ zs)) => r) -> r
assocListAppend HNil cont = cont
assocListAppend (_ :& xs) cont = assocListAppend @_ @ys @zs xs cont

rightIdListAppend :: forall xs f r. HList f xs -> ((xs ++ '[]) ~ xs => r) -> r
rightIdListAppend HNil cont = cont
rightIdListAppend (_ :& xs) cont = rightIdListAppend xs cont

type HList :: (a -> Type) -> [a] -> Type
data HList f xs where
  HNil :: HList f '[]
  (:&) :: f x -> HList f xs -> HList f (x : xs)

infixr 5 :&

deriving instance (forall x. Show (f x)) => Show (HList f xs)

hhead :: HList f (x : xs) -> f x
hhead (x :& _) = x

htail :: HList f (x : xs) -> HList f xs
htail (_ :& xs) = xs

(!!&) :: HList f xs -> Index xs x -> f x
xs !!& Z = hhead xs
xs !!& (S i) = htail xs !!& i

(++&) :: HList f xs -> HList f ys -> HList f (xs ++ ys)
HNil ++& ys = ys
(x :& xs) ++& ys = x :& (xs ++& ys)

hmap :: (forall x. f x -> g x) -> HList f xs -> HList g xs
hmap _ HNil = HNil
hmap f (x :& xs) = f x :& hmap f xs

htraverse :: Applicative m => (forall x. f x -> m (g x)) -> HList f xs -> m (HList g xs)
htraverse _ HNil = pure HNil
htraverse f (x :& xs) = liftA2 (:&) (f x) (htraverse f xs)