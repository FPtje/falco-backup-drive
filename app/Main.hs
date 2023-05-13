{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conferer qualified
import Drive.MountDrive (
  MountDriveConfig (..),
  MountingError (MountCommandFailed),
  runMountDrive,
  tryMounting,
 )
import Effectful (runEff)
import Effectful.Error.Static qualified as Error
import Effectful.FileSystem qualified as FileSystem
import Effectful.Process qualified as Process
import Effectful.Reader.Static qualified as Reader

main :: IO ()
main = do
  config <- Conferer.mkConfig "falcobackupdrive"
  diskConfig <- Conferer.fetch @MountDriveConfig config
  print diskConfig
  eMountError <-
    runEff $
      FileSystem.runFileSystem $
        Process.runProcess $
          Error.runErrorNoCallStack @MountingError $
            Reader.runReader diskConfig $
              runMountDrive tryMounting

  case eMountError of
    Left (MountCommandFailed command args stdout stderr) ->
      putStrLn $
        "Mounting command failed: "
          <> command
          <> " "
          <> unwords args
          <> "\n\nstdout:"
          <> stdout
          <> "\n\nstderr: "
          <> stderr
    Right () -> pure ()
