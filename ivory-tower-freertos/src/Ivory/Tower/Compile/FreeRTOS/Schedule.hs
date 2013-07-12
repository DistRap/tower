{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Tower.Compile.FreeRTOS.Schedule where

import GHC.TypeLits
import Control.Monad (forM_)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower.Types

import qualified Ivory.OS.FreeRTOS.Queue as Q

import Ivory.Tower.Compile.FreeRTOS.ChannelQueues
import Ivory.Tower.Compile.FreeRTOS.SharedState
import Ivory.Tower.Compile.FreeRTOS.Types

mkSystemSchedule :: [TaskNode] -> [SigNode] -> (ModuleDef, Def('[]:->()))
mkSystemSchedule tnodes _signodes = (md, initDef)
  where
  allguards = map eventGuard tnodes
  initDef = proc "freertos_towerschedule_init" $ body $ do
    -- Initialize all task guards
    mapM_ (call_ . guard_initDef) allguards
    retVoid

  md = do
    incl initDef
    -- own all task guards
    mapM_ guard_moduleDef allguards

endpointNodes :: [NodeSt a] -> ChannelId -> [NodeSt a]
endpointNodes nodes ch = filter hasref nodes
  where hasref n = elem ch (inboundChannels n)
        inboundChannels n = map unLabeled (nodest_receivers n)

-- Schedule emitter: create the emitter macro for the channels. Returns
-- failure value.
mkEmitter :: forall n area eff cs s
           . (SingI n, IvoryArea area, GetAlloc eff ~ Scope cs)
          => [TaskNode]
          -> [SigNode]
          -> Ctx -- System
          -> ChannelEmitter n area -- Codegen
          -> ConstRef s area
          -> Ivory eff IBool
mkEmitter tnodes snodes ctx emitter ref = do
    -- with all of the endpoints for chref, create an ivory
    --   monad that calls emit on each one, noting failure if it occurs
    f <- local (ival false)
    forM_ endEmitters $ \fch -> do
      s <- fch_emit fch ctx ref
      unless s (store f true)
    --   then calls notify on each of the appropriate guards
    forM_ endGuards $ \g -> guard_notify g ctx
    deref f
  where
  (endEmitters, endGuards) = mkEmitterPrims tnodes snodes emitter

mkEmitterPrims :: forall n area
               . (SingI n, IvoryArea area)
              => [TaskNode]
              -> [SigNode]
              -> ChannelEmitter n area -- Codegen
              -> ([FreeRTOSChannel area],[FreeRTOSGuard])
mkEmitterPrims tnodes snodes emitter = (chans, guards)
  where
  channel = ce_chid emitter
  ets = endpointNodes tnodes channel
  ess = endpointNodes snodes channel
  guards :: [FreeRTOSGuard]
  guards = map eventGuard ets
  chans :: [FreeRTOSChannel area]
  chans = (map (eventQueue channel (sing :: Sing n)) ets)
       ++ (map (eventQueue channel (sing :: Sing n)) ess)

mkReceiver :: forall n s eff cs area i
            . (SingI n, IvoryArea area, GetAlloc eff ~ Scope cs)
           => [TaskNode]  -- All system tasknodes
           -> [SigNode]   -- All system signodes
           -> Ctx         -- receiver execution ctx
           -> NodeSt i    -- receiving node
           -> ChannelReceiver n area  -- receiving channel
           -> Ref s area
           -> Ivory eff IBool
mkReceiver _tnodes _snodes ctx noderx chrx ref =
  fch_receive fch ctx ref
  where
  fch = eventQueue (cr_chid chrx) (sing :: Sing n) noderx

mkSigSchedule :: [TaskNode] -> [SigNode] -> SigNode -> SigSchedule
mkSigSchedule tnodes signodes tnode = SigSchedule
    { ssch_mkEmitter    = mkSigEmitter
    , ssch_mkReceiver   = mkSigReceiver
    }
  where
  mkSigEmitter :: (SingI n, IvoryArea area, GetAlloc eff ~ Scope cs)
               => ChannelEmitter n area
               -> ConstRef s area
               -> Ivory eff IBool
  mkSigEmitter emitter ref = mkEmitter tnodes signodes ISR emitter ref

  mkSigReceiver :: (SingI n, IvoryArea area, GetAlloc eff ~ Scope cs)
                => ChannelReceiver n area
                -> Ref s area
                -> Ivory eff IBool
  mkSigReceiver chrxer k = mkReceiver tnodes signodes ISR tnode chrxer k

  {-
  mkSigBody :: (forall eff cs . (GetAlloc eff ~ Scope cs) => Ivory eff ())
            -> Def('[]:->())
  mkSigBody b = proc name (body b)
    where
    name = case signalst_cname (nodest_impl tnode)  of
      Just n  -> n
      Nothing -> nodest_name tnode
  -}

mkTaskSchedule :: [TaskNode] -> [SigNode] -> TaskNode -> TaskSchedule
mkTaskSchedule tnodes signodes tnode = TaskSchedule
    { tsch_mkDataReader = mkDataReader
    , tsch_mkDataWriter = mkDataWriter
    , tsch_mkEmitter    = mkEmitter tnodes signodes User
    , tsch_mkReceiver   = mkReceiver tnodes signodes User tnode
    }

mkDataReader :: (IvoryArea area)
             => DataSink area -> Ref s area -> Ivory eff ()
mkDataReader dsnk = fdp_read fdp
  where fdp = sharedState (unDataSink dsnk)

mkDataWriter :: (IvoryArea area)
             => DataSource area -> ConstRef s area -> Ivory eff ()
mkDataWriter dsrc = fdp_write fdp
  where fdp = sharedState (unDataSource dsrc)



assembleTask :: [TaskNode] -> [SigNode] -> TaskNode -> AssembledNode TaskSt
assembleTask tnodes snodes tnode = AssembledNode
  { an_nodest = tnode
  , an_entry = entry
  , an_modules = \sysdeps -> [ taskLoopMod sysdeps, taskUserCodeMod sysdeps ]
  }
  where
  schedule = mkTaskSchedule tnodes snodes tnode
  named n = (n ++ nodest_name tnode)
  taskst = nodest_impl tnode
  taskLoopMod sysdeps = package (named "tower_task_loop_") $ do
    taskst_moddef taskst schedule
    incl entry
    depend (taskUserCodeMod sysdeps)

  taskUserCodeMod sysdeps = package (named "tower_task_usercode_") $ do
    taskst_moddef_user taskst
    mapM_ th_moddef $ taskst_taskhandlers taskst
    depend (taskLoopMod sysdeps)
    sysdeps

  entry = proc (named "tower_task_loop_") $ body $ do
    case taskst_taskinit taskst of
      Just p -> call_ p
      Nothing -> return ()
    forever $ noBreak $ do
      guard_block (eventGuard tnode) period_gcd
      mapM_ th_scheduler $ taskst_taskhandlers taskst
    where
    period_gcd = case taskst_periods taskst of
                    [] -> Q.maxWait
                    ps -> fromInteger $ foldl1 gcd ps

assembleSignal :: [TaskNode] -> [SigNode] -> SigNode -> AssembledNode SignalSt
assembleSignal tnodes snodes snode = undefined

