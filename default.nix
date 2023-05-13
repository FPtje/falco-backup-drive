{ lib, callCabal2nix
}:
let
  src = lib.cleanSourceWith rec {
    name = "falco-backup-drive-src";
    src = ./.;
    filter = path: type:
      let
        suffixAllowlist = [
          ".cabal"
          ".hs"
          "LICENSE"
        ];
        suffixDenylist = [
          "nix"
          "dist-newstyle"
          ".vscode"
        ];
      in
        ((type == "directory") ||
         (builtins.any (suffix: lib.hasSuffix suffix path) suffixAllowlist)
        ) &&
        !(builtins.any (suffix: lib.hasSuffix suffix path) suffixDenylist) &&
        # Simple library function to remove git related files.
        lib.cleanSourceFilter path type
      ;
  };
in
  callCabal2nix "falco-backup-drive" src {}
