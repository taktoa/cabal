{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Distribution.Utils.MonadCommand
  ( module Distribution.Utils.MonadCommand -- FIXME: specific export list
  ) where

import Distribution.Verbosity
import Distribution.Simple.Program.Run

-- | FIXME: doc
class Command c where
  -- | FIXME: doc
  commandRunProgram :: Verbosity -> ProgramInvocation -> c

-- | FIXME: doc
class (Monad m, Command (Action m)) => MonadCommand m where
  -- | FIXME: doc
  type Action m :: *

  -- | This is analogous to a @build@ clause in Ninja.
  need :: [m ()] -> m ()

  -- | This is analogous to a @rule@ clause in Ninja.
  runAction :: Action m -> m ()

runProgram :: (MonadCommand m) => Verbosity -> ProgramInvocation -> m ()
runProgram verb progInv = runAction (commandRunProgram verb progInv)

instance Command (IO ()) where
  commandRunProgram = runProgramInvocation

instance MonadCommand IO where
  type Action IO = IO ()
  need = sequence_
  runAction = id
