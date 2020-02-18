{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base16-bytestring, bytestring
      , criterion, deepseq, memory, random-bytestring, stdenv, tasty
      , tasty-hunit, text
      }:
      mkDerivation {
        pname = "base16";
        version = "0.1.2";
        src = ./.;
        libraryHaskellDepends = [ base bytestring text ];
        testHaskellDepends = [
          base base16-bytestring bytestring memory random-bytestring tasty
          tasty-hunit text
        ];
        benchmarkHaskellDepends = [
          base base16-bytestring bytestring criterion deepseq memory
          random-bytestring text
        ];
        homepage = "https://github.com/emilypi/base16";
        description = "RFC 4648-compliant Base16 encodings/decodings";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
