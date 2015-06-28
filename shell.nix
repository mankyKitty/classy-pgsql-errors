{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, lens, postgresql-simple, semigroups
      , stdenv, text, doctest
      }:
      mkDerivation {
        pname = "classy-pgsql-errors";
        version = "0.1.0.0";
        src = ./.;
        buildDepends = [
          base lens postgresql-simple semigroups text doctest
        ];
        description = "Lens based Optics typeclasses for including PostgreSQL Simple library";
        license = stdenv.lib.licenses.mit;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
