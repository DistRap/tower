{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Monitor
  ( handler
  , state
  , stateInit
  , unsafeStateAttrs
  , monitorModuleDef
  , Handler()
  , Monitor()
  ) where

import Ivory.Tower.Types.Unique
import Ivory.Tower.Monad.Handler
import Ivory.Tower.Monad.Monitor
import Ivory.Tower.Monad.Base

import Ivory.Language

state :: (IvoryArea a, IvoryZero a)
      => String -> Monitor e (Ref 'Global a)
state n = state' n Nothing []

stateInit :: (IvoryArea a, IvoryZero a)
          => String -> Init a -> Monitor e (Ref 'Global a)
stateInit n i = state' n (Just i) []

-- Accepts AreaAttribute for example Section "xy" to place
-- this state to "xy" memory section according to linker script
--
-- This is used in combination with memory retention
-- functions of microcontrollers, can be also used to
-- put variables into special sections of MCU like battery
-- backed ram or core coupled memory.
--
-- Marked as unsafe due to need for manually initializing
-- such variables (if needed).
unsafeStateAttrs :: (IvoryArea a, IvoryZero a)
      => String -> [AreaAttribute] -> Monitor e (Ref 'Global a)
unsafeStateAttrs n attrs = state' n Nothing attrs

state' :: (IvoryArea a, IvoryZero a)
       => String
       -> Maybe (Init a)
       -> [AreaAttribute]
       -> Monitor e (Ref 'Global a)
state' n i attrs = do
  f <- freshname n
  let a  = setMemAreaAttributes attrs $ area (showUnique f) i
  monitorModuleDef $ defMemArea a
  return (addrOf a)
