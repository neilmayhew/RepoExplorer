{ mkDerivation, base, bytestring, cmdargs, containers
, cryptohash-md5, curl, debian, directory, download-curl, fgl
, filepath, lib, parsec, unix, zlib
}:
mkDerivation {
  pname = "RepoExplorer";
  version = "0.5.0.0";
  src = lib.cleanSource ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring cmdargs containers cryptohash-md5 curl debian
    directory download-curl fgl filepath parsec unix zlib
  ];
  description = "A set of utilities for exploring Debian package repositories";
  license = lib.licenses.mit;
}
