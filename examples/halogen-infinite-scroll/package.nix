{ ps-pkgs, ... }:
  with ps-pkgs;
  { version = "1.0.0";
    dependencies =
      [ css
        halogen
        halogen-css
        halogen-subscriptions
        halogen-infinite-scroll
        js-timers
        mmorph
        pipes
        resourcet
        halogen-xterm 
      ];
    src = "src";
  }
