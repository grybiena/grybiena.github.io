{ ps-pkgs, ... }:
  with ps-pkgs;
  { version = "1.0.0";
    dependencies =
      [ ace
        css
        halogen
        halogen-canvas
        halogen-css
        halogen-subscriptions
        halogen-xterm
        leveldb
      ];
    src = "src";


    foreign."Ace".node_modules =
      npmlock2nix.v2.node_modules { src = ./.; } + /node_modules;
    foreign."Ace.Editor".node_modules =
      npmlock2nix.v2.node_modules { src = ./.; } + /node_modules;
    foreign."Ace.EditSession".node_modules =
      npmlock2nix.v2.node_modules { src = ./.; } + /node_modules;
    foreign."Ace.Config".node_modules =
      npmlock2nix.v2.node_modules { src = ./.; } + /node_modules;
    foreign."Ace.Range".node_modules =
      npmlock2nix.v2.node_modules { src = ./.; } + /node_modules;
    foreign."Ace.Marker".node_modules =
      npmlock2nix.v2.node_modules { src = ./.; } + /node_modules;

  }
