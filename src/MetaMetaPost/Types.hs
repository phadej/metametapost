{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module MetaMetaPost.Types where

import Bound
import Bound.Scope (bitraverseScope)
import Control.Monad (ap)
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Deriving (deriveEq1, deriveShow1)
import Data.Functor.Classes

import MetaMetaPost.Color
import MetaMetaPost.STLC

-------------------------------------------------------------------------------
-- Scalar type
-------------------------------------------------------------------------------

-- | /MetaMetaPost/ type.
type Ty = Type Scalar

-- | Scalar "basis" types
data Scalar
    = SNum
    | SStr
    | SPair
    | SPath
    | SPict
    | SColor
    | SStmt
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- Primitive operations
-------------------------------------------------------------------------------

-- | Nullary primitives.
--
-- /Note:/ We could use 'DataKinds', but we don't.
data Arity0

-- | Unary primitives.
data Arity1

-- | Binary primitives.
data Arity2

-- | Singleton arity type.
data SArity arity where
    Arity0 :: SArity Arity0
    Arity1 :: SArity Arity1
    Arity2 :: SArity Arity2

-- | Implicit 'SArity'.
class    IArity arity  where iarity :: SArity arity
instance IArity Arity0 where iarity = Arity0
instance IArity Arity1 where iarity = Arity1
instance IArity Arity2 where iarity = Arity2

-- | Primitive operations
data Prim arity where
    Num :: Rational -> Prim Arity0  -- @42@
    Str :: String   -> Prim Arity0  -- @btex "foo" etex@
    Clr :: Color    -> Prim Arity0  --
    Error :: String -> Prim Arity0  -- 'undefined'
    Bbox ::            Prim Arity1  -- @bbox(p)@
    Cycle ::           Prim Arity1  -- @p--cycle@
    Draw ::            Prim Arity1  -- @draw p@
    DrawC ::           Prim Arity2  -- @draw p withcolor c@
    DrawA ::           Prim Arity1  -- @drawarrow p@
    DrawAC ::          Prim Arity2  -- @drawarrow p withcolor c@
    Image ::           Prim Arity1  -- @image(s)@
    InterPoint ::      Prim Arity2  -- @p intersectionpoint q@
    Label ::           Prim Arity2  -- @label(str, z)@
    Line ::            Prim Arity2  -- @z0--z1@
    Line' ::           Prim Arity2  -- @z0--z2@
    Pair ::            Prim Arity2  -- @(x, y)@
    Scaled ::          Prim Arity2  -- @x scaled n@
    Semicolon ::       Prim Arity2  -- @x; y@
    Unfill ::          Prim Arity1  -- @unfill p@
    XPart ::           Prim Arity1  -- @xpart p@
    YPart ::           Prim Arity1  -- @ypart p@

deriving instance Eq (Prim arity)
deriving instance Show (Prim arity)

data SomePrim where
    SomePrim :: IArity arity => Prim arity -> SomePrim

-- | Equality comparison of 'SomePrim'.
somePrimEq :: forall a b. (IArity a, IArity b) => Prim a -> Prim b -> Bool
somePrimEq x y = case (iarity :: SArity a, iarity :: SArity b) of
    (Arity0, Arity0) -> x == y
    (Arity1, Arity1) -> x == y
    (Arity2, Arity2) -> x == y
    (_, _)           -> False

instance Eq SomePrim where
    SomePrim a == SomePrim b = somePrimEq a b

deriving instance Show SomePrim

-- | Type(s) of wrapped primitives.
somePrimTy :: SomePrim -> [Ty]
somePrimTy (SomePrim p) = primTy p

-- | Type(s) of primitives.
primTy :: Prim arity -> [Ty]
primTy (Num _)    = [ TyScalar SNum ]
primTy (Str _)    = [ TyScalar SStr ]
primTy (Clr _)    = [ TyScalar SColor ]
primTy (Error _)  = [ ] -- no type means @forall a. a@
primTy Pair       = [ TyScalar SNum :-> TyScalar SNum :-> TyScalar SPair ]
primTy XPart      = [ TyScalar SPair :-> TyScalar SNum ]
primTy YPart      = [ TyScalar SPair :-> TyScalar SNum ]
primTy Draw       = [ TyScalar SPath :-> TyScalar SStmt
                    , TyScalar SPict :-> TyScalar SStmt
                    ]
primTy DrawC      = [ TyScalar SPath :-> TyScalar SColor :-> TyScalar SStmt ]
primTy DrawA      = [ TyScalar SPath :-> TyScalar SStmt ]
primTy DrawAC     = [ TyScalar SPath :-> TyScalar SColor :-> TyScalar SStmt ]
primTy InterPoint = [ TyScalar SPath :-> TyScalar SPath :-> TyScalar SPair ]
primTy Unfill     = [ TyScalar SPath :-> TyScalar SStmt
                    ]
primTy Line       = [ TyScalar SPair :-> TyScalar SPair :-> TyScalar SPath ]
primTy Line'      = [ TyScalar SPath :-> TyScalar SPair :-> TyScalar SPath ]
primTy Cycle      = [ TyScalar SPath :-> TyScalar SPath ]
primTy Image      = [ TyScalar SStmt :-> TyScalar SPict ]
primTy Bbox       = [ TyScalar SPict :-> TyScalar SPath ]
primTy Label      = [ TyScalar SStr :-> TyScalar SPair :-> TyScalar SPict ]
primTy Scaled     = [ TyScalar SPath :-> TyScalar SNum :-> TyScalar SPath
                    , TyScalar SPict :-> TyScalar SNum :-> TyScalar SPict
                    , TyScalar SPair :-> TyScalar SNum :-> TyScalar SPair
                    ]
primTy Semicolon  = [ TyScalar SStmt :-> TyScalar SStmt :-> TyScalar SStmt ]



-------------------------------------------------------------------------------
-- Evaled Expr
-------------------------------------------------------------------------------

-- | Evaluated & simplified 'Expr'
--
-- The normalised form is chain of @let@s with 'MPApp' at the end:
--
-- @
-- let x1 = e1 
--     x2 = e2
--     ...
-- in b
-- @
--
-- where @e1@, @e2@... and @b@ are 'MPApp', i.e. variables
-- or prititive applications.
data MP ty a
    = MPApp (MPApp a)
    | MPLet ty (MPApp a) (Scope () (MP ty) a)
  deriving (Functor, Foldable, Traversable)

-- | A "nil" of 'MP'. Primitive application with other 'MPApp' as arguments
-- or a variable.
data MPApp a
    = MPVar a
    | MPApp0 (Prim Arity0)
    | MPApp1 (Prim Arity1) (MPApp a)
    | MPApp2 (Prim Arity2) (MPApp a) (MPApp a)
  deriving (Functor, Foldable, Traversable)

makeBound   ''MPApp
deriveEq1   ''MPApp
deriveShow1 ''MPApp

instance Applicative (MP ty) where
    pure = MPApp . pure
    (<*>) = ap

instance Monad (MP ty) where
    MPApp m >>= f = floatTop (m >>= MPVar . f)
    MPLet ty e b >>= f = mpLet ty (MPApp e >>= f) (b >>>= f)

-- | "Re-assoc" let bindings floating.
--
-- @
-- let x = (let y = e in b) in b2
-- =>
-- let y = e in let x = b in b2
-- @
--
mpLet :: ty -> MP ty a -> Scope () (MP ty) a -> MP ty a
mpLet ty (MPApp x) b         = MPLet ty x b
mpLet ty (MPLet ty' e' b') b = MPLet ty' e' $
    toScope $ mpLet ty (fromScope b') $
    fmap F b

-- | Float let bindings to top.
--
-- @
-- f (let x = e in b)
-- =>
-- let x = e in f b
-- @
--
floatTop :: MPApp (MP ty a) -> MP ty a
floatTop (MPVar x)  = x
floatTop (MPApp0 p) = MPApp (MPApp0 p)
floatTop (MPApp1 p x) = pushApp (floatTop x) where
    pushApp :: MP ty' a' -> MP ty' a'
    pushApp (MPApp e) = MPApp (MPApp1 p e)
    pushApp (MPLet ty e b) = MPLet ty e $
        toScope $ pushApp $ fromScope b

-- f (let x = e in b) (let x' = e' in b')
-- =>
-- let x = e in let x' = e' in f b b'
floatTop (MPApp2 p x y) = pushApp (floatTop x) (floatTop y) where
    pushApp :: MP ty' a' -> MP ty' a'  -> MP ty' a'
    pushApp (MPApp x') (MPApp y') = MPApp (MPApp2 p x' y')
    pushApp (MPLet ty e b) y' = MPLet ty e $
        toScope $ flip pushApp (fmap F y') $ fromScope b
    pushApp x' (MPLet ty e b) = MPLet ty e $
        toScope $ pushApp (fmap F x') $ fromScope b

deriveEq1   ''MP
deriveShow1 ''MP

instance (Eq ty, Eq a) => Eq (MP ty a) where (==) = eq1
instance (Show ty, Show a) => Show (MP ty a) where showsPrec = showsPrec1

instance Bifunctor MP where
    bimap = bimapDefault

instance Bifoldable MP where
    bifoldMap = bifoldMapDefault

instance Bitraversable MP where
    bitraverse f g = go where
        go (MPApp x)      = MPApp <$> traverse g x
        go (MPLet ty e b) = MPLet <$> f ty <*> traverse g e <*> goScope b

        goScope = bitraverseScope f g

instance (Eq a) => Eq (MPApp a) where (==) = eq1
instance (Show a) => Show (MPApp a) where showsPrec = showsPrec1
