{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PostfixOperators #-}

module Ivory.Tower.Types.Time
  ( Time
  , Microseconds(..)
  , Milliseconds(..)
  , Seconds(..)
  , toMicroseconds
  , toMilliseconds
  , microseconds
  , us
  , ms
  , seconds
  , minutes
  , hours
  , days

  , ITime
  , fromIMicroseconds
  , fromIMilliseconds
  , fromISeconds
  , toIMicroseconds
  , toIMilliseconds
  , toISeconds
  , toITime
  , prettyTime
  ) where

#if MIN_VERSION_mainland_pretty(0,6,0)
import           Text.PrettyPrint.Mainland.Class
#endif
import Text.PrettyPrint.Mainland

import Ivory.Language

class Time a where
  toMicroseconds :: a -> Integer

microseconds :: Time a => a -> Microseconds
microseconds = Microseconds . toMicroseconds

toMilliseconds :: (Time a) => a -> Integer
toMilliseconds t = (toMicroseconds t) `div` 1000

newtype Microseconds = Microseconds Integer deriving (Eq, Show, Ord)
instance Time Microseconds where
  toMicroseconds (Microseconds t) = t

us :: Integer -> Microseconds
us = Microseconds

newtype Milliseconds = Milliseconds Integer deriving (Eq, Show, Ord)
instance Time Milliseconds where
  toMicroseconds (Milliseconds t) = t * 1000

ms :: Integer -> Milliseconds
ms = Milliseconds

newtype Seconds = Seconds Integer deriving (Eq, Show, Ord)
instance Time Seconds where
  toMicroseconds (Seconds t) = t * 1000 * 1000

seconds :: Integer -> Seconds
seconds = Seconds

minutes :: Integer -> Seconds
minutes = Seconds . (*60)

hours :: Integer -> Seconds
hours = minutes . (*60)

days :: Integer -> Seconds
days = hours . (*24)

newtype ITime = ITime Sint64
  deriving ( Num, IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryOrd
           , IvoryIntegral, IvoryStore, IvoryInit, IvoryZeroVal, Bounded)

instance SafeCast ITime Sint64 where

fromIMicroseconds :: (SafeCast a Sint64) => a -> ITime
fromIMicroseconds = ITime . safeCast

fromIMilliseconds :: (SafeCast a Sint64) => a -> ITime
fromIMilliseconds = ITime . (*1000) . safeCast

fromISeconds :: (SafeCast a Sint64) => a -> ITime
fromISeconds = ITime . (*1000000) . safeCast

toIMicroseconds :: ITime -> Sint64
toIMicroseconds (ITime t) = t

toIMilliseconds :: ITime -> Sint64
toIMilliseconds (ITime t) = t `iDiv` 1000

toISeconds :: ITime -> Sint64
toISeconds (ITime t) = t `iDiv` 1000000

toITime :: (Time a) => a -> ITime
toITime t = fromIMicroseconds us'
  where
  us' :: Sint64
  us' = fromIntegral (toMicroseconds t)

prettyTime :: (Time a) => a -> String
prettyTime m = t
  where
  us' = toMicroseconds m
  t  = case us' `mod` 1000 of
    0 -> case us' `mod` 1000000 of
           0 -> (show (us' `div` 1000000)) ++ "s"
           _ -> (show (us' `div` 1000)) ++ "ms"
    _ -> (show us') ++ "us"

instance Pretty Microseconds where
  ppr m = text (prettyTime m)
