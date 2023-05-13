{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conferer qualified
import Display (displayString)
import Drive.MountDrive (
  MountDriveConfig (..),
  MountingError (..),
  runMountDrive,
  tryMounting,
 )
import Effectful (runEff)
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
        Reader.runReader diskConfig $
          runMountDrive tryMounting

  case eMountError of
    Left err -> putStrLn $ displayString err
    Right () -> pure ()
