{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Effect to run commands and deal with errors
module Command where

import Control.Monad.Catch (catchIOError)
import Display (Display (..))
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, unsafeEff_)
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import GHC.IO.Exception (ExitCode (..))
import System.Process qualified as Process

data CommandInfo = CommandInfo
  { command :: String
  , args :: [String]
  }

data CommandOutput = CommandOutput
  { stdout :: String
  , stderr :: String
  }

instance Display CommandInfo where
  display cmd =
    display cmd.command
      <> " "
      <> display (unwords cmd.args)

instance Display CommandOutput where
  display cmd =
    "stdout:\n"
      <> display cmd.stdout
      <> "\n\nstderr:\n"
      <> display cmd.stderr

data CommandError
  = CommandIOError String CommandInfo
  | CommandFailed Int CommandInfo CommandOutput

instance Display CommandError where
  display = \case
    CommandIOError err commandInfo ->
      "Unknown error running command:\n\n"
        <> display err
        <> "\nCommand was: "
        <> display commandInfo
    CommandFailed exitCode commandInfo commandOutput ->
      "Command failed with exit code "
        <> display exitCode
        <> "\nCommand was: "
        <> display commandInfo
        <> "\n"
        <> display commandOutput

data Command :: Effect

newtype instance StaticRep Command = Command ()

type instance DispatchOf Command = Static WithSideEffects

runCommand :: IOE :> es => Eff (Command : es) a -> Eff es a
runCommand = evalStaticRep $ Command ()

runProcessThrowOnError
  :: (Command :> es, Error CommandError :> es)
  => String
  -> [String]
  -> String
  -> Eff es ()
runProcessThrowOnError executable args stdin = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode executable args stdin
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure code ->
      Error.throwError $
        CommandFailed
          code
          (CommandInfo executable args)
          (CommandOutput stdout stderr)

readProcessWithExitCode
  :: (Command :> es, Error CommandError :> es)
  => String
  -> [String]
  -> String
  -> Eff es (ExitCode, String, String)
readProcessWithExitCode executable args stdin =
  unsafeEff_ (Process.readProcessWithExitCode executable args stdin)
    `catchIOError` \err ->
      Error.throwError $
        CommandIOError (show err) $
          CommandInfo executable args
