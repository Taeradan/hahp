# https://github.com/Gabriel439/haskell-nix/blob/master/README.md
# https://github.com/Gabriel439/haskell-nix/blob/master/project1/release2.nix
let
config = {
    packageOverrides = pkgs: rec {
        docker-container-hahp = pkgs.dockerTools.buildImage {
            name = "hahp-container";
            config.Cmd = [ "${haskellPackages.hahp-hydra}/bin/hahp-example" ];
        };

        haskellPackages = pkgs.haskellPackages.override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
                hahp-hydra =
                    haskellPackagesNew.callPackage ./default.nix {
                        liblapack = pkgs.liblapack;
                        openblasCompat = pkgs.openblasCompat;
                    };
            };
        };
    };
};

pkgs = import <nixpkgs> { inherit config; };

in
{ hahp-hydra = pkgs.haskellPackages.hahp-hydra;
  hahp-container = pkgs.docker-container-hahp;
}
