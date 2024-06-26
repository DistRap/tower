{-# LANGUAGE RankNTypes #-}

{- | This module is a placeholder until we have backend-specific signal
 - support.
 -}
module Ivory.Tower.Types.SignalCode where

import qualified Data.Map as Map
import Ivory.Language

data SignalCode = SignalCode
  { signalcode_signals :: Map.Map String GeneratedSignal
  , signalcode_init    :: forall eff. Ivory eff ()
  }

instance Semigroup SignalCode where
  (<>) a b = SignalCode
    { signalcode_signals = signalcode_signals a `Map.union` signalcode_signals b
    , signalcode_init    = signalcode_init a >> signalcode_init b
    }

instance Monoid SignalCode where
  mempty = SignalCode
    { signalcode_signals = Map.empty
    , signalcode_init    = return ()
    }

newtype GeneratedSignal =
  GeneratedSignal
    { unGeneratedSignal :: (forall s . Ivory (AllocEffects s) ()) -> ModuleDef
    -- ^ Unsafe signal continuation.
    }
