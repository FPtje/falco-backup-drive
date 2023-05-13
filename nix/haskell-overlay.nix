{ haskell } :
final: previous:
{
  falco-backup-drive = final.callPackage ../default.nix {};

  conferer = haskell.lib.doJailbreak previous.conferer;
}
