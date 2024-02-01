rec {
  description = "examples-halogen-xterm";
  inputs = {
    halogen-xterm.url = "github:grybiena/halogen-xterm?ref=windows";
    leveldb.url = "github:grybiena/leveldb";
    xterm.follows = "halogen-xterm/xterm";
    env.follows = "halogen-xterm/env";
    halogen-canvas = {
      url = "github:grybiena/halogen-canvas";
      inputs = {
        env.follows = "halogen-xterm/env";
      };
    };

  };

  outputs = inputs@{ env, ... }:
    env.flake-utils.lib.eachDefaultSystem (system:
      env.build-package { inherit system;
                          name = description;
                          src = ./.;
                          overlays = inputs; 
                          derive-package = ./package.nix;
                        }                
   );
}

