{ ps-pkgs, ... }:
  with ps-pkgs;
  { version = "1.0.0";
    dependencies =
      [ css
        halogen
        halogen-css
        halogen-subscriptions
        halogen-infinite-scroll
        mmorph
        pipes
        resourcet
      ];
    src = "src";
  }
