{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- | Simple typed lambda calculus - type definitions.
module MetaMetaPost.STLC (
    Expr (..),
    whnf,
    exprSize,
    Type (..),
    TypeF (..),
    Type' (..),
    liftType,
    lowerType,
    lowerTypeA,
    ) where

import Bound
import Bound.Scope (bitraverseScope)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Deriving (deriveEq1, deriveShow1, deriveEq2, deriveOrd2, deriveShow2)
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Functor.Identity (Identity (..))
import Data.String

-------------------------------------------------------------------------------
-- Expr
-------------------------------------------------------------------------------

-- | The Simple typed lambda calculus expression.
data Expr p ty a
    = Var a                                           -- ^ variable: @x@
    | App (Expr p ty a) (Expr p ty a)                 -- ^ application: @f x@
    | Lam ty (Scope () (Expr p ty) a)                 -- ^ abstraction: @\\x -> b@
    | Let ty (Expr p ty a) (Scope () (Expr p ty) a)   -- ^ let-binding: @let x = e in b@
    | Prim p                                          -- ^ primitive
  deriving (Functor, Foldable, Traversable)

instance a ~ String => IsString (Expr p ty a) where
    fromString = Var

makeBound   ''Expr
deriveEq1   ''Expr
deriveShow1 ''Expr

instance Bifunctor (Expr a) where
    bimap = bimapDefault

instance Bifoldable (Expr a) where
    bifoldMap = bifoldMapDefault

instance Bitraversable (Expr p) where
    bitraverse f g = go where
        go (Var x)      = Var <$> g x
        go (App x y)    = App <$> go x <*> go y
        go (Lam ty b)   = Lam <$> f ty <*> goScope b
        go (Let ty e b) = Let <$> f ty <*> go e <*> goScope b
        go (Prim p)     = pure (Prim p)

        goScope = bitraverseScope f g

instance (Eq p, Eq ty, Eq a) => Eq (Expr p ty a) where
    (==) = eq1

instance (Show p, Show ty, Show a) => Show (Expr p ty a) where
    showsPrec = showsPrec1

-- | Weak-head normal form.
whnf :: Expr p ty a -> Expr p ty a
whnf (App f a) = case whnf f of
    Lam _ b   -> whnf (instantiate1 a b)
    Let _ e b -> whnf (instantiate1 e b)
    f'        -> App f' a
whnf e = e

-- | The amount of sub-expressions in 'Expr'.
exprSize :: Expr p ty a -> Int
exprSize = go . fmap (const 0) where
    go :: Expr p ty Int -> Int
    go (Var _)     = 1
    go (App f x)   = 1 + go f + go x
    go (Lam _ b)   = 1 + go (instantiate1 (Var 1) b)
    go (Let _ e b) = 1 + go e + go (instantiate1 (Var 1) b)
    go (Prim _)    = 1

-------------------------------------------------------------------------------
-- Type
-------------------------------------------------------------------------------

-- | STLC types
data Type s
    = TyScalar s         -- ^ basis type
    | Type s :-> Type s  -- ^ function arrow
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

infixr 0 :->

makeBound ''Type
makeBaseFunctor ''Type

deriveEq2   ''TypeF
deriveOrd2  ''TypeF
deriveShow2 ''TypeF

instance Eq a => Eq1 (TypeF a) where liftEq = liftEq2 (==)
instance Ord a => Ord1 (TypeF a) where liftCompare = liftCompare2 compare
instance Show a => Show1 (TypeF a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Eq a, Eq s) => Eq (TypeF a s) where (==) = eq1
instance (Ord a, Ord s) => Ord (TypeF a s) where compare = compare1
instance (Show a, Show s) => Show (TypeF a s) where showsPrec = showsPrec1

-------------------------------------------------------------------------------
-- Extended Type
-------------------------------------------------------------------------------

-- | Extended type, which can contain type variables.
newtype Type' s = Type' (Var Int (TypeF s (Type' s)))
  deriving (Eq, Ord, Show)

-- | Lift 'Type' into 'Type''.
liftType :: Type s -> Type' s
liftType = cata $ \c -> Type' (F c)

-- | Lower 'Type'' into 'Type' in an 'Applicative' context.
lowerTypeA :: Applicative m => (Int -> m (Type s)) -> Type' s -> m (Type s)
lowerTypeA f = go where
    go (Type' (B i))   = f i
    go (Type' (F tyf)) = fmap embed (traverse go tyf)

-- | Lower 'Type'' to 'Type'.
lowerType :: (Int -> Type s) -> Type' s -> Type s
lowerType f = runIdentity . lowerTypeA (Identity . f)
