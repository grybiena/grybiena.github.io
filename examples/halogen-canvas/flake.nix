rec {
  description = "examples-halogen-canvas";
  inputs = {
    env.url = "github:grybiena/purescript-environment";  
    halogen-canvas = {
      url = "github:grybiena/halogen-canvas";
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

