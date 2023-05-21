{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Backup.ExternalDisk (ExternalDiskBackup, run, runExternalDiskBackup, loop) where

import Backup.RSync (RSync)
import Backup.RSync qualified as RSync
import Config.Backup.ExternalDisk (ExternalDiskBackupConfig (..))
import Drive.MountDrive (MountDrive)
import Drive.MountDrive qualified as MountDrive
import Effectful (Eff, Effect, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.TH (makeEffect)

-- | Waits for the a disk to appear, mounts it, backs up a directory, and then unmounts the disk
-- again
data ExternalDiskBackup :: Effect where
  Run :: ExternalDiskBackupConfig -> ExternalDiskBackup m ()

makeEffect ''ExternalDiskBackup

runExternalDiskBackup
  :: (MountDrive :> es, RSync :> es, Concurrent :> es)
  => Eff (ExternalDiskBackup : es) a
  -> Eff es a
runExternalDiskBackup = interpret $ \_ -> \case
  Run config -> do
    Reader.runReader config.mountConfig $ do
      MountDrive.blockUntilDiskAvailable
      RSync.run config.rsyncConfig
      MountDrive.unmount

-- | Waits for a disk to appear, mounts it, backs up a directory, unmounts the disk again, waits
-- until the disk is gone, and then repeats the process
loop
  :: (ExternalDiskBackup :> es, Reader ExternalDiskBackupConfig :> es, MountDrive :> es, Concurrent :> es)
  => ExternalDiskBackupConfig
  -> Eff es ()
loop config =
  go
 where
  go = do
    run config
    Reader.runReader config.mountConfig MountDrive.blockUntilDiskGone

    go
