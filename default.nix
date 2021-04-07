{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base16-bytestring, bytestring
      , criterion, deepseq, lib, primitive, QuickCheck, random-bytestring
      , tasty, tasty-hunit, tasty-quickcheck, text, text-short
      }:
      mkDerivation {
        pname = "base16";
        version = "0.3.0.1";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring deepseq primitive text text-short
        ];
        testHaskellDepends = [
          base base16-bytestring bytestring QuickCheck random-bytestring
          tasty tasty-hunit tasty-quickcheck text text-short
        ];
        benchmarkHaskellDepends = [
          base base16-bytestring bytestring criterion deepseq
          random-bytestring text
        ];
        homepage = "https://github.com/emilypi/base16";
        description = "Fast RFC 4648-compliant Base16 encoding";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
