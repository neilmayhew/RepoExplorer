{ lib, mkDerivation
, base, bytestring, cmdargs, containers, cryptohash, debian, directory
, download-curl, fgl, filepath, MissingH, parsec, unix, zlib
}:
mkDerivation {
  pname = "RepoExplorer";
  version = "0.5.0.0";
  src = lib.cleanSource ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring cmdargs containers cryptohash debian directory
    download-curl fgl filepath MissingH parsec unix zlib
  ];
  description = "A set of utilities for exploring Debian package repositories";
  license = lib.licenses.mit;
}
