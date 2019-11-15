{ pkgs, commonLib, chairmanScript, ... }:

let chairman-runner = chairmanScript {
      chairman-config = {
        enable     = true;
        k          = 2160;
        timeout    = 300;
        maxBlockNo = 30;
        topology = commonLib.mkEdgeTopology {};

      };
    };
in {
  name = "chairmans-cluster-test";
  nodes = {
    machine = { config, pkgs, ... }: {
      imports = [
        ../.
      ];
      services.cardano-cluster = {
        enable = true;
        node-count = 3; ## This must match nixos/scripts.nix:mkChairmanScript
      };
    };
  };
  testScript = ''
    startAll
    $machine->waitForOpenPort(3001);
    $machine->waitForOpenPort(3002);
    $machine->waitForOpenPort(3003);
    $machine->succeed("netstat -pltn | systemd-cat --identifier=netstat --priority=crit");
    $machine->succeed("${chairman-runner} 2>&1 | systemd-cat --identifier=chairman --priority=crit");
  '';

}
