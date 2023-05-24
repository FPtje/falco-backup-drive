{ haskell } :
final: previous:
{
  falco-backup-drive = final.callPackage ../default.nix {};

  conferer = haskell.lib.doJailbreak previous.conferer;
  postgresql-simple = haskell.lib.doJailbreak previous.postgresql-simple;
  persistent = haskell.lib.doJailbreak previous.persistent;
}
