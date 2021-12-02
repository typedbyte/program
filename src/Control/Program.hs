{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Program
-- Copyright   :  (c) Michael Szvetits, 2021
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  typedbyte@qualified.name
-- Stability   :  stable
-- Portability :  portable
--
-- Types and functions for representing programs which run in a specific
-- environment and are able to integrate bracket-like operations.
-----------------------------------------------------------------------------
module Control.Program
  ( -- * Program Representation
    Program
  , runProgram
    -- * Resource Handling
  , bracket
  , bracketE
  , manage
  , local
    -- * Environment Handling
  , Has(from)
  , ask
  , pull
  , pullWith
  )
  where

-- base
import qualified Control.Exception as E
import Control.Monad.IO.Class (MonadIO, liftIO)

-- | Represents a program that produces a value of type @a@ when running in an
-- environment @e@. The required content of the environment is usually described
-- by declaring 'Has' constraints on @e@.
--
-- Turning an 'IO' action into a 'Program' is usually done by using 'liftIO'.
newtype Program e a = Program { unProgram :: e -> forall b. (a -> IO b) -> IO b }

instance Functor (Program e) where
  fmap f (Program g) =
    Program $ \env cont ->
      g env (cont . f)
  {-# INLINE fmap #-}

instance Applicative (Program e) where
  pure r =
    Program $ \_ cont ->
      cont r
  {-# INLINE pure #-}
  
  Program f <*> Program g =
    Program $ \env cont ->
      f env $ \h ->
        g env (cont . h)
  {-# INLINE (<*>) #-}

instance Monad (Program e) where
  Program f >>= g =
    Program $ \env cont ->
      f env $ \a ->
        unProgram (g a) env cont
  {-# INLINE (>>=) #-}

instance MonadFail (Program e) where
  fail = liftIO . fail
  {-# INLINE fail #-}

instance MonadIO (Program e) where
  liftIO m =
    Program $ \_ cont ->
      m >>= cont
  {-# INLINE liftIO #-}

-- | Runs a program in a given environment @e@.
runProgram :: e -> Program e a -> IO a
runProgram env (Program f) = f env pure

-- | Acquire a resource, use it, and then release the resource automatically
-- after the program ends.
bracket
  :: IO a        -- ^ The computation which acquires the resource.
  -> (a -> IO b) -- ^ The computation which releases the resource.
  -> Program e a -- ^ The computation which uses the resource.
bracket create destroy =
  Program $ \_ cont ->
    E.bracket create destroy cont
{-# INLINE bracket #-}

-- | A version of 'bracket' where the acquisition and release actions may
-- consult the environment @e@.
bracketE :: (e -> IO a) -> (e -> a -> IO b) -> Program e a
bracketE create destroy =
  Program $ \env cont ->
    E.bracket (create env) (destroy env) cont
{-# INLINE bracketE #-}

-- | Integrates a continuation into a 'Program', which is useful for integrating
-- existing bracket-like continuations (often named @with...@).
manage :: (forall b. (a -> IO b) -> IO b) -> Program e a
manage f =
  Program $ \_ cont ->
    f cont
{-# INLINE manage #-}

-- | Runs a sub-program within a program, which is useful for fine-grained
-- resource handling (i.e., resources acquired by the sub-program are released
-- after the sub-program ends, not at the end of the whole program).
local :: Program e a -> Program e a
local program =
  Program $ \env cont ->
    runProgram env program >>= cont
{-# INLINE local #-}

-- | Demands that a specific value of type @t@ must be present in the
-- environment @e@.
class e `Has` t where
  -- | Extracts a value of type @t@ from the environment @e@.
  from :: e -> t

instance {-# OVERLAPPABLE #-} e `Has` e where
  from = id
  {-# INLINE from #-}

-- | Gets the environment.
ask :: Program e e
ask = Program $ \env cont -> cont env
{-# INLINE ask #-}

-- | Extracts a specific value of type @t@ from the environment.
pull :: e `Has` t => Program e t
pull = Program $ \env cont -> cont (from env)
{-# INLINE pull #-}

-- | Extracts a specific value of type @t@ from the environment and extracts
-- some 'IO' action from it. This is useful if the environment contains a
-- record of 'IO' functions (e.g., a function which returns a handle).
pullWith :: e `Has` t => (t -> IO a) -> Program e a
pullWith f =
  Program $ \env cont ->
    f (from env) >>= cont
{-# INLINE pullWith #-}