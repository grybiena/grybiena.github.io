rec {
  description = "examples-halogen-infinite-scroll";
  inputs = {
    env.url = "github:grybiena/purescript-environment";  
    halogen-xterm = {
      url = "github:grybiena/halogen-xterm";
      inputs = {
        env.follows = "env";
      };
    };
    xterm.follows = "halogen-xterm/xterm";
    halogen-infinite-scroll = {
      url = "github:grybiena/halogen-infinite-scroll?ref=interface-changes";
      inputs = {
        env.follows = "env";
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

