{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module MetaMetaPost.TypeInferrer (
    typeinfer,
    -- * Internal
    -- ** Infer monad
    Infer,
    runInfer,
    IS (..),
    initialIS,
    -- ** Infer actions
    newVar,
    tellConstraint,
    tellTypeEq,
    tellTypeChoice,
    tellError,
    solve,
    -- ** Types
    TyConstraint (..),
    ) where

import Bound
import Control.Monad.State.Strict
import Data.Bitraversable
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.Functor.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.List (sort)
import Data.Monoid (Endo (..))
import Data.Semigroup ((<>))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import MetaMetaPost.STLC

-- | Type constraints.
data TyConstraint s
    = TyEq (Type' s) (Type' s)                -- ^ type equality, tilde in Haskell
    | TyChoice (Type' s) (NonEmpty (Type s))  -- ^ "ad-hoc" polymorphism
  deriving (Eq, Ord, Show)


-- | Infer is non-deterministic 'StateT' monad which let us
--
-- *  make new type variables,
--
-- * and collect 'TyConstraint's.
--
-- * collect warnings and errors. (TODO: make a type)
--
type Infer s = StateT (IS s) NonEmpty

-- | Run 'Infer' return first non-errorneous @a@ if possible.
runInfer :: Infer s a -> Either [String] a
runInfer m =
    case partitionEithers (toList (fmap f (runStateT m initialIS))) of
        (_, (x : _)) -> Right x
        ((e : _), _) -> Left e
        ([], [])     -> Left ["no solutions"]
  where
    -- partition results into errorneous and not
    f (x, IS _ _ es) = case appEndo es [] of
        [] -> Right x
        xs -> Left xs

-- | The state of 'Infer' monad.
data IS s = IS !Int !(Endo [TyConstraint s]) !(Endo [String])

-- | Empty or initial 'IS'.
initialIS :: IS s
initialIS = IS 0 mempty mempty

-------------------------------------------------------------------------------
-- Simple actions
-------------------------------------------------------------------------------

-- | Create new type variable.
newVar :: Infer s (Type' s)
newVar = do
    IS i cs ws <- get
    put (IS (i + 1) cs ws)
    pure (Type' (B i))

tellConstraint :: TyConstraint s -> Infer s ()
tellConstraint c = modify' $ \(IS i cs ws) ->
    IS i (cs <> Endo (c :)) ws

-- | Record type equality constraint ('TyEq').
tellTypeEq :: Ord s => Type' s -> Type' s -> Infer s ()
tellTypeEq a b | a < b = tellConstraint (TyEq a b)
tellTypeEq a b         = tellTypeEq b a

-- | Record type choice constraint ('TyChoice').
tellTypeChoice :: Type' s -> [Type s] -> Infer s ()
tellTypeChoice a [] = do
    ty <- newVar
    tellConstraint (TyEq a ty)
tellTypeChoice a (b : bs) = tellConstraint (TyChoice a (b :| bs))

-- | Record an error, /Note/ this doesn't abort the computation.
tellError :: String -> Infer s ()
tellError err = modify' $ \(IS i cs es) ->
    IS i cs (es <> Endo (err :))

checkOk :: Infer s Bool
checkOk = do
    IS _ _ ws <- get
    pure (null $ appEndo ws [])

-------------------------------------------------------------------------------
-- Solve
-------------------------------------------------------------------------------

-- | Solve constraints.
--
-- Will always produce /some/ 'Map', but it might not contain
-- bindings for all variables. In that case there should be recorded
-- errors in 'Infer'.
--
-- We could use <http://hackage.haskell.org/package/unification-fd unification-fd>,
-- but we make this never fail: when we encounter unsatisfiable constraint,
-- we record it (as error), but proceed anyway.
--
solve :: forall s. (Show s, Ord s) => Type s -> [TyConstraint s] -> Infer s (Map Int (Type s))
solve defType = go mempty mempty . ordNub . sort where
    -- See: https://en.wikipedia.org/wiki/Unification_(computer_science)#A_unification_algorithm
    --
    -- TODO: below much be outdated.
    --
    -- * @nVar@ is the number of variables that occur more than once in the
    -- equation set
    --
    -- * @nLhs@ is the number of function symbols and constants on
    -- the left hand sides of potential equations,
    --
    -- * @nEqn@ is the number of equations, and
    --
    -- * @nChoice@ is the number of choice equations.
    --
    go :: Map Int (Type s)
       -> Map Int (Type' s)
       -> [TyConstraint s]
       -> Infer s (Map Int (Type s))
    go subst subst' [] = post subst subst'

    -- Only choice
    --
    -- @nChoice@ goes down.
    --
    go subst subst' (TyChoice ty (x :| []) : cs) = do
        go subst subst' (TyEq ty (liftType x) : cs)

    -- Have to make choice
    --
    -- @nChoice@ goes down.
    --
    go subst subst' (TyChoice ty xs : cs) = do
        ok <- checkOk
        if ok
            then do
                x <- lift xs
                go subst subst' (ordNub $ TyEq ty (liftType x) : cs)
            else
                go subst subst' cs

    -- Delete
    --
    -- @nEqn@ goes down
    --
    go subst subst' (TyEq a b : cs) | a == b =
       go subst subst' cs

    -- (Occurs) Check
    --
    -- @nEqn@ goes down
    --
    -- Happens in 'post'. TODO: test!
    {-
    go subst subst' (TyEq (Type' (B i)) ty : cs) | Set.member i (vars ty) = do
        tellError "occurs check"
        go subst subst' cs

    -}

    -- Eliminate
    --
    -- @nEqn@ goes down
    --
    go subst subst' (TyEq (Type' (B i)) ty : cs) = do
        go subst (Map.insert i ty (substInF i ty subst')) (substInCs i ty cs)

    -- Swap
    --
    -- @nLhs@ goes down
    --
    go subst subst' (TyEq ty@(Type' (F _)) ty'@(Type' (B _)) : cs) =
        go subst subst' (ordNub $ TyEq ty' ty : cs)

    -- Decompose
    --
    -- @nLhs@ goes down
    --
    go subst subst' (TyEq (Type' (F (a :->$ b)))  (Type' (F (a' :->$ b'))) : cs) = do
        go subst subst' (ordNub $ TyEq a a' : TyEq b b' : cs)

    -- Conflict
    --
    -- @nEqn@ goes down
    --
    go subst subst' (TyEq (Type' (F ty)) (Type' (F ty')) : cs) = do
        unless (ty == ty') $
            tellError $ "Cannot match " ++ show ty ++ " and " ++ show ty'

        go subst subst' cs

    -- Post processing
    -- Recursively apply bindings.
    --
    -- TODO: verify we cannot loop here.
    post
        :: Map Int (Type s)
        -> Map Int (Type' s)
        -> Infer s (Map Int (Type s))
    post subst subst' = case Map.minViewWithKey subst' of
        Nothing -> pure subst
        Just ((i, ty), m) -> do
            ty' <- lowerTypeA (f i) ty
            post (Map.insert i ty' subst) (substInF i (liftType ty') m)
      where
        f i j | i == j = do
            tellError "occurs check"
            pure defType
        f i j = case Map.lookup j subst of
            Just ty -> pure ty
            Nothing -> case Map.lookup j subst' of
                -- No such var anywhere: use default
                Nothing  -> pure defType
                -- otherwise recurse
                Just ty' -> lowerTypeA (f i) ty'

substInF :: Functor f => Int -> Type' s -> f (Type' s) -> f (Type' s)
substInF i ty = fmap (substInTy i ty)

substInCs :: Int -> Type' s -> [TyConstraint s] -> [TyConstraint s]
substInCs i ty = map $ \c -> case c of
    TyEq a b      -> TyEq (substInTy i ty a) (substInTy i ty b)
    TyChoice a xs -> TyChoice (substInTy i ty a) xs

substInTy :: Int -> Type' s -> Type' s -> Type' s
substInTy i ty = go where
    go ty'@(Type' (B j)) | i == j    = ty
                         | otherwise = ty'
    go (Type' (F f))                 = Type' (F (fmap go f))

{-
vars :: Type' s -> Set Int
vars (Type' (B i)) = Set.singleton i
vars (Type' (F f)) = foldMap vars f
-}

-- | /TODO:/ Doesn't preserve the order!
ordNub :: Ord a => [a] -> [a]
ordNub = go Set.empty where
    go !_ [] = []
    go !s (x:xs)
        | x `Set.member` s = go s xs
        | otherwise        = let !s' = Set.insert x s in x : go s' xs

-------------------------------------------------------------------------------
-- Inference
-------------------------------------------------------------------------------

-- | Infer a type of the expression.
typeinfer :: forall p s a. (Show p, Show a, Show s, Ord s)
          => Type s                         -- ^ default type
          -> (p -> [Type s])                -- ^ type(s) of primitives
          -> Expr p () a                    -- ^ expression to type
          -> Either [String] (Expr p (Type s) a)  -- ^ typed expression or list of errors
typeinfer defType pr expr = runInfer $ do
    -- First we change all types to unsolved type variables
    e0 <- bitraverse (\() -> newVar) pure expr

    -- After that we change the type of free variables to unsolved type variables
    -- too.
    --
    -- TODO: collect into the map,
    -- then we can return what type each free variable should be. (named) holes!
    e1 <- bitraverse pure (\_ -> newVar) e0

    -- Collect constraints
    _ <- walk e1
    IS _ constraints' _ <- get
    let constraints = appEndo constraints' []

    -- Solve constraints
    solution <- solve defType constraints

    -- After everytrhing is solved, we lookup the concrete types.
    case bitraverse (lower solution) pure e0 of
        Nothing -> error "unsolved something"
        Just x -> pure x
  where
    walk :: Expr p (Type' s) (Type' s) -> Infer s (Type' s)
    walk (Var ty) = pure ty
    walk (App f x) = do
        tyF <- walk f
        tyX <- walk x
        b <- newVar
        tellTypeEq tyF (Type' (F (tyX :->$ b)))
        pure b
    walk (Let ty e b) = do
        tyE <- walk e
        tellTypeEq ty tyE
        walk (instantiate1 (Var ty) b)
    walk (Lam ty b) = do
        tyB <- walk (instantiate1 (Var ty) b)
        pure (Type' (F (ty :->$ tyB)))
    walk (Prim p) = do
        ty <- newVar
        tellTypeChoice ty (pr p)
        pure ty

    lower :: Map Int (Type s) -> Type' s -> Maybe (Type s)
    lower m = go where
        go (Type' (B i)) = Map.lookup i m
        go (Type' (F t)) = fmap embed (traverse go t)
