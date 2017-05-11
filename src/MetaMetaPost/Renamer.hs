module MetaMetaPost.Renamer where

import Control.Monad.State.Strict
import Data.Bitraversable

import MetaMetaPost.Types

-- | Tag each 'MPLet'.
--
-- Making each let-binding uniquely named, though we don't have names!
--
-- This is a prepartion step for pretty printing. Not very interesting
-- on its own.
--
rename :: MP s a -> MP (Int, s) a
rename e = evalState (bitraverse f pure e) 0
  where
    f ty = do
        i <- get
        put (i + 1)
        pure (i, ty)
