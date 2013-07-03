{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Ivory.Tower.Test.FooBarSimple where

import Ivory.Language
import Ivory.Tower

[ivory|
struct foo_state
  { foo_member :: Stored Uint8
  }
|]

[ivory|
struct bar_state
  { bar_member :: Stored Uint8
  }
|]

[ivory|
struct some_other_type
  { some_other_member :: Stored Uint8
  }
|]

fooBarTypes :: Module
fooBarTypes = package "fooBarTypes" $ do
  defStruct (Proxy :: Proxy "foo_state")
  defStruct (Proxy :: Proxy "bar_state")

someOtherModule :: Module
someOtherModule= package "someOtherModule" $ do
  defStruct (Proxy :: Proxy "some_other_type")


fooSourceTask :: DataSource (Struct "foo_state") -> Task ()
fooSourceTask fooSource = do
    fooWriter <- withDataWriter fooSource "fooSource"
    p <- withPeriod 250
    taskBody $ \sch -> do
      state <- local (istruct [])
      eventLoop sch $ onTimer p $ \_now -> do
        v <- deref (state ~> foo_member)
        store (state ~> foo_member) (v + 1)
        writeData sch fooWriter (constRef state)

barSourceTask :: (SingI n)
              => ChannelSource n (Struct "bar_state") 
              -> Task ()
barSourceTask barSource = do
    barEmitter <- withChannelEmitter barSource "barSource"
    p <- withPeriod 125
    taskBody $ \sch -> do
      state <- local (istruct [])
      eventLoop sch $ onTimer p $ \_now -> do
        v <- deref (state ~> bar_member)
        store (state ~> bar_member) (v + 1)
        emit_ sch barEmitter (constRef state)

fooBarSinkTask :: (SingI n)
               => DataSink (Struct "foo_state")
               -> ChannelSink n (Struct "bar_state")
               -> Task ()
fooBarSinkTask fooSink barSink = do
  barReceiver <- withChannelReceiver barSink "barSink"
  fooReader   <- withDataReader    fooSink "fooSink"
  taskBody $ \sch -> do
    latestFoo <- local (istruct [])
    latestSum <- local (ival 0)
    eventLoop sch $ onChannel barReceiver $ \latestBar -> do
      readData sch fooReader latestFoo
      bmember <- deref (latestBar ~> bar_member)
      fmember <- deref (latestFoo ~> foo_member)
      store latestSum (bmember + fmember)

fooBarTower :: Tower ()
fooBarTower = do
  (source_f, sink_f) <- dataport
  (source_b, sink_b) <- channel

  task "fooSourceTask"  $ fooSourceTask source_f
  task "barSourceTask"  $ barSourceTask source_b
  task "fooBarSinkTask" $ fooBarSinkTask sink_f sink_b

  addDepends fooBarTypes
  addModule fooBarTypes
  addModule someOtherModule