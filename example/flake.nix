rec {
  description = "example";
  inputs = {
    env.url = "github:grybiena/purescript-environment";  
    examples-halogen-xterm = {
      url = "path:../examples/halogen-xterm";
      inputs = {
        env.follows = "env";
        xterm.follows = "xterm";
        halogen-xterm.follows = "halogen-xterm";
        halogen-canvas.follows = "halogen-canvas";
      };
    };
    examples-halogen-infinite-scroll = {
      url = "path:../examples/halogen-infinite-scroll";
      inputs = {
        env.follows = "env";
        xterm.follows = "xterm";
        halogen-xterm.follows = "halogen-xterm";
        halogen-infinite-scroll.follows = "halogen-infinite-scroll";
      };
    };
    halogen-canvas = {
      url = "github:grybiena/halogen-canvas";
      inputs = {
        env.follows = "env";
      };
    };
    leveldb = {
      url = "github:grybiena/leveldb";
      inputs = {
        env.follows = "env";
      };
    };
    halogen-xterm = {
      url = "github:grybiena/halogen-xterm?ref=windows";
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
      let
        overlays = [ ];

        pkgs = import env.nixpkgs {
          inherit system overlays;
          config.allowBroken = true;
        };

        ps-tools = env.ps-tools.legacyPackages.${system};
        purs-nix = env.purs-nix {
          inherit system; 
          overlays = with inputs; env.gen-overlays { inherit pkgs system; } inputs;
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
          module = "Main";
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


