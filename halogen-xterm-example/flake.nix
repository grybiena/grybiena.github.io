rec {
  description = "halogen-xterm-example";

  inputs = {

    halogen-xterm.url = "github:grybiena/halogen-xterm?ref=windows";
    leveldb.url = "github:grybiena/leveldb";
    xterm.follows = "halogen-xterm/xterm";
    env.follows = "halogen-xterm/env";
    halogen-canvas = {
      url = "git+file:../../halogen-canvas?ref=main";
      inputs = {
        env.follows = "halogen-xterm/env";
      };
    };

  };
  outputs = inputs@{ env, ... }:
    env.flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ ];

        pkgs = import env.nixpkgs {
          inherit system overlays;
          config.allowBroken = true;
        };

        ps-tools = env.ps-tools.legacyPackages.${system};
        purs-nix = env.purs-nix {
          inherit system; 
          overlays = with inputs; env.gen-overlays { inherit pkgs system; } {
            inherit halogen-canvas halogen-xterm xterm leveldb;
          };
        };

        package = import ./package.nix purs-nix;
  
        ps =
          purs-nix.purs { inherit (package) dependencies;
                          dir = ./.;
                        };

        bundle = {
          esbuild = {
            outfile = "main.js";
            platform = "browser";
            minify = "true";
            };
          module = "Example";
        };


      in 
         { packages.default = ps.bundle bundle;

           devShells.default = 
             pkgs.mkShell
               { packages = with pkgs; [
                   nodejs
                   (ps.command { inherit bundle;}) 
                   ps-tools.for-0_15.purescript-language-server
                   purs-nix.esbuild
                   purs-nix.purescript
                 ];
               };
         }
   );
}

