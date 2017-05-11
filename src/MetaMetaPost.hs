{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | /MetaMetaPost/ is a small EDSL to produce /MetaPost/ programs.
-- Using /Haskell/ as a metalanguage is very convenient.
--
-- This is a small example of @bound@ package usage.
--
-- For demo "programs" see  "MetaMetaPost.Demos" module.
--
module MetaMetaPost (
    -- * Pipeline
    UExpr,
    UExpr',
    metametapost,
    metametapostIO,
    -- * Steps
    typeinfer,
    typecheck,
    simplify,
    rename,
    prettyprint,
    -- * DSL
    --
    -- | Convenient operators to define various 'UExpr'essions.
    module MetaMetaPost.DSL,
    -- * Types
    module MetaMetaPost.Color,
    module MetaMetaPost.Error,
    module MetaMetaPost.STLC,
    module MetaMetaPost.Types,
    ) where

import Bound (closed)
import Data.Bifunctor (bimap)
import Data.Foldable (traverse_, toList)
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Data.Void (Void, vacuous)

import qualified Text.PrettyPrint.Leijen as PP

import MetaMetaPost.Color
import MetaMetaPost.DSL
import MetaMetaPost.Error
import MetaMetaPost.PrettyPrinter
import MetaMetaPost.Renamer
import MetaMetaPost.STLC
import MetaMetaPost.Simplifier
import MetaMetaPost.TypeChecker
import MetaMetaPost.TypeInferrer
import MetaMetaPost.Types

-- | Closed untyped MetaMetaPost expression.
type UExpr = UExpr' Void

-- | Almost 'UExpr'.
type UExpr' a = Expr SomePrim () a

-------------------------------------------------------------------------------
-- Pipeline
-------------------------------------------------------------------------------

-- | Process 'UExpr' into /MetaPost/ code.
metametapost :: UExpr -> Either [String] PP.Doc
metametapost e = do
    te <- typeinfer (TyScalar SNum) somePrimTy e
    tys <- bimap (:[]) id $ typecheck somePrimTy (vacuous te)
    unless (any (== TyScalar SStmt) tys) $
        throwError (map (("Invalid main type: " ++) . show) $ toList tys)
    mp <- bimap (:[]) (rename . simplifyMP) $ simplify te
    mp' <- maybe (throwError ["Not-closed"]) pure $ closed mp
    pure (prettyprint mp')

-- | IO wrapper around 'metametapost'.
metametapostIO :: FilePath -> Expr SomePrim () Void -> IO ()
metametapostIO fp e = case metametapost e of
    Right x -> if fp == "" then print x else writeFile fp (show x ++ "\n")
    Left es -> do
        putStrLn $ "Errors (" ++ show (length es) ++ "):"
        traverse_ putStrLn es
