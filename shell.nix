let 
    config = {
      packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = haskellPackagesNew: haskellPackagesOld: rec {
            uploadmask = haskellPackagesNew.callPackage ./. {};
            gerippe = haskellPackagesNew.callPackage ./../../gerippe {};
          };
        };
      };
    };
    pkgs = import <nixpkgs> { inherit config; };
in  pkgs.haskellPackages.uploadmask.env
