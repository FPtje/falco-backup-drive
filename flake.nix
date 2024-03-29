{
  description = "falco-backup-drive, manage my backups through my drive";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ self.overlays.${system}.falcoBackupDrive ];
      };
    in
    {
      overlays.falcoBackupDrive = (import ./nix/overlay.nix);

      packages = {
        falcoBackupDrive = pkgs.falcoBackupDrivePkgs.haskellPackages.falco-backup-drive;
        default = self.packages.${system}.falcoBackupDrive-static;
      };

      apps.default = {
        type = "app";
        program = "${self.packages.${system}.falcoBackupDrive}/bin/falco-backup-drive";
      };

      devShell = with pkgs.falcoBackupDrivePkgs.haskellPackages; shellFor {
        packages = p: [ p.falco-backup-drive ];
        buildInputs = [
          cabal-install
          cabal-fmt
          fourmolu_0_13_0_0
          haskell-language-server
        ] ++ pkgs.falcoBackupDrivePkgs.haskellPackages.falco-backup-drive.runtimeInputs;
      };
    }
  );
}
