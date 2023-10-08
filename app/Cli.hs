{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Defines the command line parser
module Cli where

import Control.Monad.IO.Class (MonadIO (..))
import Effectful (Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import Options.Applicative

data CommandLineArguments
  = -- | Run the program regularly to perform its main activity, i.e. regularly making backups
    Run
  | -- | Mount all drives and then exit
    Mount
  | -- | Unmount all drives and then exit
    Unmount

-- | Optparse-applicative based command line parser that returns
commandLineParser :: Parser CommandLineArguments
commandLineParser =
  subparser
    ( command "run" (info (pure Run) (progDesc "Run indefinitely to perform regular backups"))
        <> command "mount" (info (pure Mount) (progDesc "Mount all drives mentioned in config and exit"))
        <> command "unmount" (info (pure Unmount) (progDesc "Unmount all drives mentioned in config and exit"))
    )
    -- Passing no arguments should just run the application
    <|> pure Run

-- | The full cli parser
commandLineOptions :: ParserInfo CommandLineArguments
commandLineOptions =
  info
    (commandLineParser <**> helper)
    ( fullDesc <> progDesc "Runs Falco's personal backups"
    )

-- | An effect that retursn the passed command line arguments
data Cli :: Effect where
  GetCommandLineArguments :: Cli m CommandLineArguments

makeEffect ''Cli

-- | Run the Cli effect in IO, parsing the command line arguments given to the application
runCliIO :: IOE :> es => Eff (Cli : es) a -> Eff es a
runCliIO = interpret $ \_ -> \case
  GetCommandLineArguments ->
    liftIO $ execParser commandLineOptions
