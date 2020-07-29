![Haskell CI](https://github.com/neilmayhew/RepoExplorer/workflows/Haskell%20CI/badge.svg)

RepoExplorer
============

*A set of utilities for exploring Debian package repositories*

The following utilities are available:

**RepoList**

*List and optionally check a repository's contents*

**DependencyRoots**

*Calculate a minimal set of packages on your system that is sufficient to
install all the other currently-installed packages via dependency*

Further utilities will be added later.

Using
-----

Run each utility with `--help` for a detailed description of how to use it.

Installing
----------

### Debian/Ubuntu

#### Locally-built binaries

On Debian/Ubuntu, most of the build dependencies exist as pre-built OS packages. The build process is:

```Bash
sudo apt-get install -y --no-install-recommends \
	ghc cabal-install \
	libghc-{cmdargs,cryptohash,debian,fgl,missingh,curl,feed,tagsoup,xml}-dev

cabal update
cabal install --only-dependencies
cabal configure
cabal build
```

This will produce binaries in `dist/build/RepoList/RepoList` and `dist/build/DependencyRoots/DependencyRoots`. Copy these to `~/bin`, `/usr/local/bin` or any other convenient place.

You can find the definitive list of build dependencies in `debian/control`, omitting `debhelper`, `haskell-devscripts` and `cdbs`, which are needed only for building packages, and `libghc-download-curl` which is no longer a system package and is installed using `cabal`.

#### Locally-built package

You may find a Debian/Ubuntu package called `repoexplorer` in an unofficial repo somewhere. However, it's easy to build your own:

```Bash
sudo apt-get install -y --no-install-recommends \
	devscripts fakeroot haskell-debian-utils \
	libwww-perl file

git submodule update --init

cd haskell-download-curl
origtargz
sudo apt-get-build-depends -y
debuild -b -us -uc
debuild clean
cd ..

sudo dpkg -i libghc-download-curl-dev_*.deb
sudo apt-get-build-depends -y
debuild -b -us -uc
debuild clean
```

This will produce a `repoexplorer_*.deb` file in the parent directory. Install it with `sudo dpkg -i`, or sign the changes file with `debsign` and upload it to a repository with `dput`.

Since the intermediate package `libghc-download-curl-dev` is only a build dependency and not a runtime dependency, it doesn't need to be distributed to users of the `repoexplorer` package.

**Note:** On older systems the `origtargz` command may not exist. However, an equivalent shell function can be defined like this:

```Bash
origtargz()
{
	uscan --download-current-version --rename
	tar -xf ../*.orig.tar.gz --strip-components=1
}
```

#### Source package

If you want to build packages in a cleanroom environment such as `pbuilder`, you can create source packages by omitting the `apt-get-build-depends` commands and using `debuild -nc -S -sa -us -uc` instead of `debuild -b -us -uc` (although make sure your working directory is clean first).

You'll need to make the `libghc-download-curl-dev` binary available when building `repoexplorer`.

### NixOS

There's a `default.nix` in the top-level directory, so to build binaries, run:

```Bash
nix-build
```

The binaries will then be available via `result/bin`.

To make `repoexplorer` installable as a package on your system, add the following to `~/.nixpkgs/config.nix`:

```Nix
{
  packageOverrides = pkgs:
  {
    repoexplorer = pkgs.callPackage ../path/to/RepoExplorer {};
  };
}
```

Then install it with:

```Bash
nix-env -f '<nixpkgs>' -iA repoexplorer
```

### Building from source on any platform

First you need some basic Haskell development tools installed. The easiest way to do this is to install the [Haskell Platform](https://www.haskell.org/platform/) for your OS. Note that on **Linux** you should use the Generic installer since the distro package may be too old, or may not exist. Also, you'll need to install `libgmp-dev` before running the Generic installer.

To build the dependent Haskell libraries, you'll need various C/C++ tools and libraries that they use. On **Mac**, they're all included with the command-line development tools. On **Debian/Ubuntu**, they can be installed with:

```Bash
sudo apt-get install -y --no-install-recommends \
	gcc lib{c,z,bz2,curl4-openssl}-dev
```

On **Windows** you may be able to install them using Cygwin.

Run `cabal --version` to see which version you have, and then use the corresponding set of commands below.

It takes a while to build the dependencies, but the build itself is relatively short. It produces binaries in the `dist-newstyle` or `dist` subdirectory. Copy these to `~/bin`, `/usr/local/bin` or any other convenient place:

```Bash
sudo install -p $(find dist* -type f -perm -100) /usr/local/bin
```

#### Cabal 3.x or later ####

Then execute the following commands:

```Bash
cabal update
cabal build -j
```

#### Cabal 2.x ####

Then execute the following commands:

```Bash
cabal v2-update
cabal v2-build -j
```

#### Cabal 1.x ####

Then execute the following commands:

```Bash
cabal sandbox init
cabal update
cabal install -j --only-dependencies
cabal build -j
cabal sandbox delete
```
