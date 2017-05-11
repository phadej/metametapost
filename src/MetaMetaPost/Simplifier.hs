{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module MetaMetaPost.Simplifier (
    simplify,
    simplifyMP,
    ) where

import Bound
import Bound.Scope (hoistScope, transverseScope)
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

-- | Simplify 'MP'.
--
-- * Strip unused let-bindings
--
-- * Collapse bindings (sharing)
--
simplifyMP :: (Eq a, Eq ty) => MP ty a -> MP ty a
simplifyMP = share . unusedBinds
  where
    -- let _ = e in b 
    -- =>
    -- b
    unusedBinds :: MP ty a -> MP ty a
    unusedBinds e@MPApp {} = e
    unusedBinds (MPLet ty e b) = case closed (fromScope b') of
        Just x -> x
        Nothing -> MPLet ty e b'
      where
        b' = hoistScope unusedBinds b
    
    -- This is super ugly implementation atm.
    --
    -- let x = e in let y = e in b x y
    -- =>
    -- let x = e in b x x
    share :: (Eq a, Eq ty) => MP ty a -> MP ty a
    share e@MPApp {} = e
    share (MPLet ty e b) = MPLet ty e (toScope (go (B ()) (F <$> e) (fromScope $ hoistScopeEq share b)))
      where
        -- replace matching let bindings 
        go :: Eq a
           => Var () a          -- variable
           -> MPApp (Var () a)  -- variable value
           -> MP ty (Var () a)  -- expression to process
           -> MP ty (Var () a)
        go _ _ x@(MPApp _)       = x
        go v x (MPLet ty' e' b')
            | x == e'            = instantiate1 (return v) b'' 
            | otherwise          = MPLet ty' e' b''
          where
            b'' = toScope (go (F <$> v) (F <$> x) (fromScope b'))

-- | 'hoistScope' but with 'Eq' constraint.
hoistScopeEq :: (Functor f, Eq a, Eq b, Eq (g a))
             => (forall x. Eq x => f x -> g x) -> Scope b f a -> Scope b g a
hoistScopeEq t (Scope b) = Scope $ t (fmap t <$> b)

{-
hoist2Scope :: (Eq b, Eq (g a), Eq a, Functor f, Functor h, Monad g)
            => (forall x. Eq x => h x -> f x -> g x)
            -> h a -> Scope b f a -> Scope b g a
hoist2Scope t x (Scope b) = Scope $ t (F . return <$> x) (fmap (t x) <$> b)
-}
