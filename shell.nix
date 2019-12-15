let
  nixpkgsPath = <nixpkgs>;
  pkgs = import nixpkgsPath {};

  easyPurescript = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "9a8d138663c5d751e3a84f1345166e1f0f760a07";
    sha256 = "1c0mqn4wxh4bmxnf6hgrhk442kl2m9y315wik87wrw2ikb7s1szf";
  }) { inherit pkgs; };

  psPkgs = import ../release.nix { nixpkgs = pkgs; };

  # yarn2nixSrc = pkgs.fetchFromGitHub {
  #   owner = "Profpatsch";
  #   repo = "yarn2nix";
  #   rev = "919012b32c705e57d90409fc2d9e5ba49e05b471";
  #   sha256 = "1f9gw31j7jvv6b2fk5h76qd9b78zsc9ac9hj23ws119zzxh6nbyd";
  # };

  # y2nLib = "${yarn2nixSrc}/nix-lib";

  # yarn2nix = import yarn2nixSrc {};

in pkgs.mkShell {
  buildInputs = [
    # psPkgs.haskellPackages.spago
    easyPurescript.spago
    easyPurescript.inputs.purs
    pkgs.nodejs
    pkgs.jq
    pkgs.yarn
  ];
}
