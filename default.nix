{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, cmdargs, containers
      , cryptohash, debian, directory, download-curl, fgl, filepath
      , MissingH, parsec, stdenv, unix, zlib
      }:
      mkDerivation {
        pname = "RepoExplorer";
        version = "0.5.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base bytestring cmdargs containers cryptohash debian directory
          download-curl fgl filepath MissingH parsec unix zlib
        ];
        description = "A set of utilities for exploring Debian package repositories";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
