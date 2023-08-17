# Setup - before build

1) Install nix -> https://nixos.org/download.html
2) brew install ghc cabal-install


# Build

`nix build`

# Run Application

`./result/bin/encode-decode-drainer  --metrics-update-interval 10 --prometheus-port 9093 --file ~/Downloads/bigFile.txt`
