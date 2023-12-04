{ pkgs }:

final: prev:
with pkgs.haskell.lib;
let
  streamly-bytestring-repo = pkgs.fetchFromGitHub {
    owner = "psibi";
    repo = "streamly-bytestring";
    rev = "94920f7c1ed4221fc4e829c8362243107a3c2d63";
    hash = "sha256-mgBnP/IdOCvdVoWyxgyKbNduQ5A4AtdjQ3LQQpFDWtc=";
  };

  streamly-repo = pkgs.fetchFromGitHub {
    owner = "composewell";
    repo = "streamly";
    rev = "13f91aad8c3d7cb3b81dd74599ecbfe38cfc6a6e";
    hash = "sha256-Esl56HVfs2YxKAB3j2wCRxJ7UE9R9YEZlBoQl934TUA=";
  };

  hw-kafka-client-repo = pkgs.fetchFromGitHub {
    owner = "haskell-works";
    repo = "hw-kafka-client";
    rev = "e80c3220c46f4b6b2a1cd5007fca5e6df7f22ba1";
    sha256 = "sha256-M4ZHXI7gMFM0R0vAxE3ZVERDLoL0MAVtCvLAF759xO4=";
  };

  inherit (pkgs.haskell.lib) enableCabalFlag disableCabalFlag appendBuildFlag doHaddock;

in
{
  encode-decode =
    appendConfigureFlags
      (justStaticExecutables
        (final.callCabal2nix "encode-decode" ./. { }))
      [ "-fopt" ];

  streamly-core =
    doJailbreak (final.callCabal2nix "streamly-core" "${streamly-repo}/core" {});

  streamly =
    if pkgs.stdenv.isDarwin
    then
      addBuildDepends
        (doJailbreak (final.callCabal2nix "streamly" streamly-repo {}))
        [pkgs.darwin.apple_sdk.frameworks.Cocoa]
    else 
      doJailbreak (final.callCabal2nix "streamly" streamly-repo {});

  streamly-bytestring =
    doJailbreak (dontCheck (final.callCabal2nix "streamly-bytestring" streamly-bytestring-repo { }));

  hw-kafka-client = dontCheck (final.callCabal2nix "hw-kafka-client" hw-kafka-client-repo {});
}
