{ nixpkgs ? <nixpkgs> }:

let hsPkgs = import ./default.nix { inherit nixpkgs; };
in hsPkgs.default.shellFor {
  buildInputs = with hsPkgs.pkgs.haskellPackages; [ hpack ghcid brittany ];
}
