{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.Tower.Types.Chan where

import Data.Kind (Type)
import qualified Ivory.Tower.AST as AST
import Ivory.Language

data Chan (a :: Area Type) = Chan AST.Chan
  deriving (Eq)

newtype ChanInput (a :: Area Type) = ChanInput (Chan a)
  deriving (Eq)

newtype ChanOutput (a :: Area Type) = ChanOutput (Chan a)
  deriving (Eq)
