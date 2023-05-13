final: previous:
{
  falcoBackupDrivePkgs = {
    haskellOverlay = final.callPackage ./haskell-overlay.nix {};

    haskellPackages =
      previous.haskell.packages.ghc94.extend final.falcoBackupDrivePkgs.haskellOverlay;

    staticHaskellPackages =
      previous.pkgsStatic.haskell.packages.ghc94.extend final.falcoBackupDrivePkgs.haskellOverlay;
  };
}
