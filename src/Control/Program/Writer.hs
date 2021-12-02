{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Program.Writer
-- Copyright   :  (c) Michael Szvetits, 2021
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- Types and functions for handling appendable output in the environment of a
-- 'Program'.
-----------------------------------------------------------------------------
module Control.Program.Writer
  ( -- * Writer Effect
    Writer(..)
    -- * Program-based Writer
  , newWriter
  , tell
  ) where

-- base
import Data.IORef (modifyIORef', newIORef, readIORef)

import Control.Program (Has, Program, pullWith)

-- | A record of functions which represents the operations on an appendable output.
newtype Writer w = Writer { writeValue :: w -> IO () }

-- | Creates a new record of functions for appendable output, backed by an 'Data.IORef.IORef'.
--
-- Returns the record of functions and an action which reads the accumulated
-- output, usually used after running a corresponding 'Program' with the 'Writer'
-- in its environment.
newWriter :: Monoid w => IO (Writer w, IO w)
newWriter = do
  ref <- newIORef mempty
  let writer = Writer
        { writeValue = \w -> modifyIORef' ref (<> w) }
  pure (writer, readIORef ref)

-- | Produces the output @w@. In other words, @w@ is appended to the accumulated output.
tell :: e `Has` Writer w => w -> Program e ()
tell = pullWith . flip writeValue