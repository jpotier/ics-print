# An ical printer for mutt

Add it to your home-configuration and set up your mailcap to refer to ics-print.

``` nix
{
  inputs.ics-print.url = "github:asd";
  outputs = { self, nixpkgs, home-manager, ... } @ inputs: {
    homeConfigurations = {
      "name@system" = home-manager.lib.homeManagerConfiguration {
        system = "x86_64-linux";
        configuration = { pkgs, ... }: {
          nixpkgs.overlays = [ (self: super: {
            ics-print = inputs.ics-print.defaultPackage.x86_64-linux;
          })];
          home.file.".mailcap".text = ''
            ..
            text/calendar; ${pkgs.ics-print}/bin/ics-print %s; copiousoutput
            ..
          '';
        };
      };
    };
  };
}
```
