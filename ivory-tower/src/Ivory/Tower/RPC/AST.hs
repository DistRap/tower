{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Ivory.Tower.RPC.AST where

import Data.Monoid
import Ivory.Language

data SM (f :: Area) (t :: Area) =
  SM
    { sm_start  :: [Stmt t]
    , sm_blocks :: [Block f t]
    , sm_end    :: [Stmt t]
    }

instance Monoid (SM f t) where
  mempty      = SM [] [] []
  mappend a b = SM (sm_start a <> sm_start b)
                   (sm_blocks a <> sm_blocks b)
                   (sm_end a <> sm_end b)

data Stmt (t :: Area)
  = SLifted (forall s . Ivory (AllocEffects s) IBool)
  | SEmit (forall s . ConstRef  s t)

data Block (f :: Area) (t :: Area) =
  Block
    { block_ref :: (forall s . Ref s f)
    , block_stmts :: [Stmt t]
    }

-- User API to build SMs:
start :: Stmt t -> SM f t
start s = mempty { sm_start = [s] }

block :: (forall s . Ref s f) -> [Stmt t] -> SM f t
block ref stmts = mempty { sm_blocks = [Block ref stmts] }

end :: Stmt t -> SM f t
end s = mempty { sm_end = [s] }

liftIvory :: (forall s . Ivory (AllocEffects s) ()) -> Stmt t
liftIvory i = SLifted (i >> return false)

check :: (forall s . Ivory (AllocEffects s) IBool) -> Stmt t
check i = SLifted i

