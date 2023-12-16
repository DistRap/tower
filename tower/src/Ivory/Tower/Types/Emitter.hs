{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.Emitter where

import Data.Kind (Type)
import Ivory.Language

newtype Emitter (a :: Area Type) = Emitter
  { emit :: forall s eff. ConstRef s a -> Ivory eff ()
  }
