{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MetaMetaPost.Simplifier (simplify) where

import Bound
import Bound.Scope (transverseScope)
import Control.Monad.Except

import MetaMetaPost.Types
import MetaMetaPost.STLC

-- | Simplify 'Expr' into 'MP',
-- especially cutting the 'App' and 'Lam',
-- and inlining non scalar 'Let's.
--
-- This /shouldn't/ fail on type-correct terms.
--
simplify :: Expr SomePrim (Type s) a -> Either String (MP s a)
simplify (Var a)                = pure (MPApp (MPVar a))
simplify (Prim (SomePrim p))    = simplifyPrim0 p
simplify (Lam _ _)              = throwError "Non-applied lambda abstraction"
simplify (Let (TyScalar s) e b) = mpLet s <$> simplify e <*> transverseScope simplify b
simplify (Let (_ :-> _) e b)    = simplify (instantiate1 e b)
simplify (App f x)              = case whnf f of
    Lam _ b                   -> simplify (instantiate1 x b)
    Prim (SomePrim p)         -> simplifyPrim1 p x
    App (Prim (SomePrim p)) y -> simplifyPrim2 p y x
    _                         -> throwError $ "Non-primitive application"

simplifyPrim0 :: forall arity s a. IArity arity
             => Prim arity
             -> Either String (MP s a)
simplifyPrim0 p = case iarity :: SArity arity of
    Arity0 -> pure (MPApp (MPApp0 p))
    _      -> throwError $ "Non-0-arity primitive: " ++ show p

simplifyPrim1 :: forall arity s a. IArity arity
                => Prim arity
                -> Expr SomePrim (Type s) a
                -> Either String (MP s a)
simplifyPrim1 p x = case iarity :: SArity arity of
    Arity0 -> throwError $ "Application of 0-arity primitive: " ++ show p
    Arity1 -> floatTop . MPApp1 p . pure <$> simplify x
    Arity2 -> throwError $ "Partial application of 2-arity primitive: " ++ show p

simplifyPrim2 :: forall arity s a. IArity arity
                => Prim arity
                -> Expr SomePrim (Type s) a
                -> Expr SomePrim (Type s) a
                -> Either String (MP s a)
simplifyPrim2 p x y = case iarity :: SArity arity of
    Arity0 -> throwError $ "Application of 0-arity primitive: " ++ show p
    Arity1 -> throwError $ "Over-application of 1-arity primitive: " ++ show p
    Arity2 -> (\x' y' -> floatTop (MPApp2 p (pure x') (pure y')))
        <$> simplify x
        <*> simplify y
