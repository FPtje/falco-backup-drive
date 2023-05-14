{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conferer qualified
import Display (displayString)
import Drive.MountDrive (
  MountDriveConfig (..),
  MountingError (..),
  blockUntilDiskAvailable,
  runMountDrive,
 )
import Effectful (runEff)
import Effectful.Concurrent qualified as Concurrent
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static qualified as Reader

main :: IO ()
main = do
  config <- Conferer.mkConfig "falcobackupdrive"
  diskConfig <- Conferer.fetch @MountDriveConfig config
  putStrLn $ displayString diskConfig

  eMountError <-
    runEff $
      Error.runErrorNoCallStack @MountingError $
        Concurrent.runConcurrent $
          Reader.runReader diskConfig $
            runMountDrive blockUntilDiskAvailable

  case eMountError of
    Left err -> putStrLn $ displayString err
    Right () -> pure ()
