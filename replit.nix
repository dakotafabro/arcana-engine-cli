{ pkgs }: {
  deps = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.zlib                # ← adds the missing C library dependency
    pkgs.haskellPackages.aeson
    pkgs.haskellPackages.text
    pkgs.haskellPackages.http-conduit
    pkgs.haskellPackages.ansi-terminal
    pkgs.haskellPackages.directory
    pkgs.haskellPackages.mtl
    pkgs.haskellPackages.optparse-applicative
    pkgs.haskellPackages.bytestring
    pkgs.haskellPackages.containers
    pkgs.haskellPackages.random
    pkgs.haskellPackages.time
    pkgs.haskellPackages.dotenv
  ];
}