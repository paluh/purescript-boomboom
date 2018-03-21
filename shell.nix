let
  pkgs = import <nixpkgs> {};
in
  pkgs.stdenv.mkDerivation {
    name = "my-shell";
    buildInputs = [
      pkgs.haskellPackages.purescript
      pkgs.nodejs
      pkgs.nodePackages.jshint
      pkgs.nodePackages.bower
      pkgs.postgresql
      pkgs.pkgconfig
      pkgs.psc-package
      pkgs.python35
      pkgs.systemd
      pkgs.stack
    ];
    shellHook = ''
      export PATH=./node_modules/.bin:$PATH
      export EDITOR=vim
    '';
  }
