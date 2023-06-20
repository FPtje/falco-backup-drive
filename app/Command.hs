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
import System.Directory (findExecutable)
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
  | CommandDoesNotExist String

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
    CommandDoesNotExist command ->
      "The executable '"
        <> display command
        <> "' does not exist. Please make sure the program is installed."

data Command :: Effect

newtype instance StaticRep Command = Command ()

type instance DispatchOf Command = Static WithSideEffects

runCommand :: IOE :> es => Eff (Command : es) a -> Eff es a
runCommand = evalStaticRep $ Command ()

runProcessThrowOnError
  :: (Error.HasCallStack, Command :> es, Error CommandError :> es)
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

runSudoProcessThrowOnError
  :: (Error.HasCallStack, Command :> es, Error CommandError :> es)
  => String
  -> [String]
  -> String
  -> Eff es ()
runSudoProcessThrowOnError executable args stdin = do
  mbExecutablePath <- unsafeEff_ $ findExecutable executable
  case mbExecutablePath of
    Nothing -> Error.throwError $ CommandDoesNotExist executable
    Just executablePath -> runProcessThrowOnError "sudo" (executablePath : args) stdin

runSudoProcess
  :: (Error.HasCallStack, Command :> es, Error CommandError :> es)
  => String
  -> [String]
  -> String
  -> Eff es (ExitCode, String, String)
runSudoProcess executable args stdin = do
  mbExecutablePath <- unsafeEff_ $ findExecutable executable
  case mbExecutablePath of
    Nothing -> Error.throwError $ CommandDoesNotExist executable
    Just executablePath -> readProcessWithExitCode "sudo" (executablePath : args) stdin

readProcessWithExitCode
  :: (Error.HasCallStack, Command :> es, Error CommandError :> es)
  => String
  -> [String]
  -> String
  -> Eff es (ExitCode, String, String)
readProcessWithExitCode executable args stdin = do
  mbExecutablePath <- unsafeEff_ $ findExecutable executable
  case mbExecutablePath of
    Nothing -> Error.throwError $ CommandDoesNotExist executable
    Just executablePath ->
      unsafeEff_ (Process.readProcessWithExitCode executablePath args stdin)
        `catchIOError` \err ->
          Error.throwError $
            CommandIOError (show err) $
              CommandInfo executable args
