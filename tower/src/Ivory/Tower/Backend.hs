{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Tower.Backend where

import Data.Kind (Type)
import Ivory.Language
import qualified Ivory.Tower.AST as AST
import Ivory.Tower.Types.Emitter
import Ivory.Tower.Types.Unique

data SomeHandler backend = forall a. SomeHandler (TowerBackendHandler backend a)

class TowerBackend backend where
  -- XXX should probably be type families, not data families, and maybe at the
  -- top-level (without relying on the class).

  -- Type correponds to the channel type
  data TowerBackendCallback  backend :: Area Type -> Type
  data TowerBackendEmitter   backend :: Type
  -- Type correponds to the channel type
  data TowerBackendHandler   backend :: Area Type -> Type
  data TowerBackendMonitor   backend :: Type
  data TowerBackendOutput    backend :: Type

  callbackImpl :: IvoryArea a
               => backend
               -- Callback identifier, used to construct full callback name
               -> Unique
               -- Implementation
               -> (forall s s'. ConstRef s' a -> Ivory (AllocEffects s) ())
               -> TowerBackendCallback backend a
  emitterImpl :: (IvoryArea b, IvoryZero b)
              => backend
              -> AST.Emitter
              -> [TowerBackendHandler backend b]
              -> (Emitter b, TowerBackendEmitter backend)
  handlerImpl :: (IvoryArea a, IvoryZero a)
              => backend
              -> AST.Handler
              -> [TowerBackendEmitter backend]
              -> [TowerBackendCallback backend a]
              -> TowerBackendHandler backend a
  monitorImpl :: backend
              -> AST.Monitor
              -> [SomeHandler backend]
              -- Contains the state variable declarations for the monitor
              -> ModuleDef
              -> TowerBackendMonitor backend
  towerImpl :: backend
            -> AST.Tower
            -> [TowerBackendMonitor backend]
            -> TowerBackendOutput backend
