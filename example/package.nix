{ ps-pkgs, ... }:
  with ps-pkgs;
  { version = "1.0.0";
    dependencies =
      [ examples-halogen-xterm 
        examples-halogen-infinite-scroll
        examples-halogen-canvas
        xterm
        halogen-xterm
        halogen-infinite-scroll
      ];
    src = "src";

  }
