{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Program.State
-- Copyright   :  (c) Michael Szvetits, 2021
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- Types and functions for handling mutable state in the environment of a
-- 'Program'.
-----------------------------------------------------------------------------
module Control.Program.State
  ( -- * State Effect
    State(..)
    -- * Program-based State
  , get
  , put
  , modify
  , modify'
    -- * IO-based State
  , newState
  , modifyState
  , modifyState'
  ) where

-- base
import Data.IORef (newIORef, readIORef, writeIORef)

import Control.Program (Has, Program, pullWith)

-- | A record of functions which represents the operations on a mutable value.
data State s = State
  { readState  :: IO s       -- ^ Gets the current state.
  , writeState :: s -> IO () -- ^ Replaces the state with a new value.
  }

-- | Creates a new record of functions for mutable state, backed by an 'Data.IORef.IORef'.
newState :: s -> IO (State s)
newState s = do
  ref <- newIORef s
  pure $
    State
      { readState  = readIORef ref
      , writeState = writeIORef ref
      }

-- | Modifies the state, using the provided function.
modifyState :: State s -> (s -> s) -> IO ()
modifyState state f =
  readState state >>= writeState state . f

-- | A strict version of 'modifyState'.
modifyState' :: State s -> (s -> s) -> IO ()
modifyState' state f = do
  s <- readState state
  let s' = f s
  s' `seq` writeState state s'

-- | Gets the current state.
get :: e `Has` State s => Program e s
get = pullWith readState

-- | Replaces the state with a new value.
put :: e `Has` State s => s -> Program e ()
put = pullWith . flip writeState

-- | Modifies the state, using the provided function.
modify :: e `Has` State s => (s -> s) -> Program e ()
modify = pullWith . flip modifyState

-- | A strict version of 'modify'.
modify' :: e `Has` State s => (s -> s) -> Program e ()
modify' = pullWith . flip modifyState'