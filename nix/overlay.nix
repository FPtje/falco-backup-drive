final: previous:
{
  falcoBackupDrivePkgs = {
    haskellOverlay = import ./haskell-overlay.nix;

    haskellPackages =
      previous.haskell.packages.ghc94.extend final.falcoBackupDrivePkgs.haskellOverlay;

    staticHaskellPackages =
      previous.pkgsStatic.haskell.packages.ghc94.extend final.falcoBackupDrivePkgs.haskellOverlay;
  };
}
