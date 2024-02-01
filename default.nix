{ lib, pkgs, config, ... }:
with lib;
let
  tuya = pkgs.haskellPackages.callCabal2nix "tuya" ./. { };
  settings = pkgs.writeText "tuya.yaml" (builtins.toJSON cfg.settings);
  cfg = config.services.tuya;
in {
  options.services.tuya = {
    enable = mkEnableOption "Tuya Home Assistant integration";
    settings = mkOption {
      type = types.attrs;
      default = {};
    };
  };
  config = mkIf cfg.enable {
    systemd.services.tuya = {
      wantedBy = [ "multi-user.target" ];
      after = [ "mosquitto.service" ];
      serviceConfig = {
        ExecStart = "${tuya}/bin/tuya ${settings}";
        Restart = "always";
        RestartSec = 5;
        CPUAffinity = "16-31";
      };
    };
  };
}
