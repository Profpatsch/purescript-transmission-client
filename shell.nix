let
  nixpkgsPath = <nixpkgs>;
  pkgs = import nixpkgsPath {};

  easyPurescript = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "907d60008a0d50b183a95d4e723444e643c706dc";
    sha256 = "0cif0di9wa1m32q6jl69hh8amki2lrn1i5j5dw1sbsxmqlfzrkj0";
  });

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
    psPkgs.haskellPackages.spago
    easyPurescript.inputs.purs
    pkgs.nodejs
    pkgs.yarn
  ];
}
