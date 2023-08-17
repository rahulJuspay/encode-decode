{ pkgs }:

final: prev:
with pkgs.haskell.lib;
let
  streamly-bytestring-repo = pkgs.fetchFromGitHub {
    owner = "psibi";
    repo = "streamly-bytestring";
    rev = "v0.1.4";
    sha256 = "sha256-KEUTkgzUrdIrU603xbceEk/fa161i3r1IPb4XYVwFCQ=";
    fetchSubmodules = true;
  };

  inherit (pkgs.haskell.lib) enableCabalFlag disableCabalFlag appendBuildFlag doHaddock;

in
{
  encode-decode =
    appendConfigureFlags
      (justStaticExecutables
        (final.callCabal2nix "encode-decode" ./. { }))
      [ "-fopt" ];

  streamly =
    if pkgs.stdenv.isDarwin
    then pkgs.haskell.lib.addBuildDepend final.streamly_0_8_3 pkgs.darwin.apple_sdk.frameworks.Cocoa
    else final.streamly_0_8_3;

  streamly-bytestring =
    doJailbreak (dontCheck (final.callCabal2nix "streamly-bytestring" streamly-bytestring-repo { }));
}
