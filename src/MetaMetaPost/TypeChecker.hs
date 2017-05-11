{-# LANGUAGE FlexibleContexts #-}
module MetaMetaPost.TypeChecker (typecheck) where

import Bound
import Control.Monad.Except
import Data.Either (partitionEithers)
import Data.Foldable  (toList)
import Data.List (intercalate, nub)
import Data.List.NonEmpty (NonEmpty (..))

import MetaMetaPost.STLC

-- | Typecheck the 'Expr'. Return a list of possible types.
--
-- This also checks that the 'Expr' doesn't contain 'Error' terms.
typecheck :: (Eq s, Show s, Show p)
          => (p -> [Type s])                     -- ^ type of primitives
          -> Expr p (Type s) (Type s)            -- ^ expression to typecheck
          -> Either String (NonEmpty (Type s))   -- ^ a non-empty list of possible types, or an error.
typecheck pr = stripErrors' . runExceptT . go where
    go = stripErrors . runExceptT . go'

    stripErrors xs = case partitionEithers (toList xs) of
        ([], [])       -> throwError $ "Empty result" -- should never happen
        (es, [])       -> throwError $ intercalate ";" (nub es)
        (_,  (r : rs)) -> lift (r :| rs)

    stripErrors' xs = case partitionEithers (toList xs) of
        ([], [])       -> throwError $ "Empty result" -- should never happen
        (es, [])       -> throwError $ intercalate ";" (nub es)
        (_,  (r : rs)) -> pure (r :| rs)

    go' (Var ty) = pure ty
    go' (App f x) = do
        tyF <- go f
        tyX <- go x
        case tyF of
            a :-> b
                | a == tyX -> pure b
                | otherwise -> throwError $ "cannot match " ++ show a ++ " and " ++ show tyX
            _ -> throwError $ "type of f isn't a function"
    go' (Lam ty b) = do
        ty' <- go (instantiate1 (Var ty) b)
        pure (ty :-> ty')
    go' (Let ty e b) = do
        ty' <- go e
        unless (ty == ty') $ 
            throwError $ "cannot match " ++ show ty ++ " and " ++ show ty'
        go (instantiate1 (Var ty) b)
    go' (Prim p) = case pr p of
        [] ->  throwError $ "Typeless primitive " ++ show p
        (t : ts) ->  lift (t :| ts)
