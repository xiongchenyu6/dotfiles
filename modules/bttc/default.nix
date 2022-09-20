{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.services.bttc;
in
{
  imports = [ ];
  options.services.bttc = {
    enable = mkEnableOption "Enables the go bttc service";

    mainnet = mkEnableOption "weather to use mainnet";

    dynamicUser = mkEnableOption "weather to use dynamic user";

    prometheus = mkEnableOption "weather to enable prometheus metrics export";

    nodeDir = mkOption {
      description = lib.mdDoc "Data directory.";
      default = "node";
      type = types.str;
    };

    nodekeyDir = mkOption {
      description = lib.mdDoc "Data directory.";
      default = "./bttc/nodekey";
      type = types.str;
    };

    bttcDir = mkOption {
      description = lib.mdDoc "Data directory.";
      default = "./bttc";
      type = types.str;
    };

    deliveryHomeDir = mkOption {
      description = lib.mdDoc "Data directory.";
      default = "./deliveryd";
      type = types.str;
    };

    dataDir = mkOption {
      description = lib.mdDoc "Data directory.";
      default = "./bttc/data";
      type = types.str;
    };

    infuraKey = mkOption {
      description = lib.mdDoc "Data directory.";
      default = "dc39d257c9d94e37801677d9b066824c";
      type = types.str;
    };

    tronGridApiKey = mkOption {
      description = lib.mdDoc "Data directory.";
      default = "4bfa950b-a637-4286-954c-611f8303efb1";
      type = types.str;
    };

    bttcSnapShot = mkOption {
      description = lib.mdDoc "bttc snapshot Data directory.";
      default = null;
      type = types.nullOr types.str;
    };

    deliverySnapShot = mkOption {
      description = lib.mdDoc "delivery snapshot Data directory.";
      default = null;
      type = types.nullOr types.str;
    };

  };

  config =
    let

      conf-base = "${pkgs.launch}/${if cfg.mainnet then "mainnet" else "testnet-1029"}/without-sentry";
      bttc-gensis = "${conf-base}/bttc/genesis.json";
      bttc-static-nodes = "${conf-base}/bttc/static-nodes.json";
      delivery-genesis = "${conf-base}/delivery/config/genesis.json";
      delivery-seeds = importTOML "${conf-base}/delivery/delivery-seeds.txt";

      DELIVERY_BTTC_RPC_URL = "http://bttc0:8545";
      DELIVERY_ETH_RPC_URL = if cfg.mainnet then "https://mainnet.infura.io/v3/${cfg.infuraKey}" else "https://goerli.infura.io/v3/${cfg.infuraKey}";
      BSC_RPC_URL = if cfg.mainnet then "https://bsc-dataseed.binance.org/" else "https://data-seed-prebsc-1-s1.binance.org:8545/";
      TRON_RPC_URL = if cfg.mainnet then "grpc.trongrid.io:50051" else "47.252.19.181:50051";
      TRON_GRID_URL = if cfg.mainnet then "https://tronevent.bt.io/" else "https://test-tronevent.bt.io";

      update_toml = k: v: file: ''sed -i '/${k} =/c${k} = ${v}' ${file}'';

      serviceConfig = {
        User = "bttc";
        Restart = "on-failure";
        RestartSec = "5s";
        WorkingDirectory = "/var/lib/bttc";
        DynamicUser = if cfg.dynamicUser then "yes" else "no";
        RuntimeDirectory = "bttc";
        RuntimeDirectoryMode = "0755";
        StateDirectory = "bttc";
        StateDirectoryMode = "0700";
        LogsDirectory = "bttc";
        CacheDirectory = "bttc";
        CacheDirectoryMode = "0750";
        SystemCallArchitectures = "native";
        Type = "simple";
        KillSignal = "SIGINT";
        TimeoutStopSec = 120;
      };
    in
    mkIf
      cfg.enable
      {
        services.rabbitmq.enable = true;

        networking.firewall = {
          allowedTCPPorts = [ 30303 26627 26660 26656 8546 8545 ];
          allowedUDPPorts = [ 30303 8546 8545 ];
        };
        systemd.services = {
          bttc = {
            inherit serviceConfig;
            description = "Bttc Service Daemon";
            wantedBy = [ "multi-user.target" ];
            after = [ "networking.target" ];
            startLimitIntervalSec = 500;
            startLimitBurst = 5;
            environment = {
              BTTC_DIR = "${cfg.bttcDir}";
              DATA_DIR = "${cfg.dataDir}";
              NODE_KEY = "${cfg.nodekeyDir}";
              ADDRESS = "aaaaa";
            };
            preStart = ''
              # create bttc and keystore directory
              mkdir -p $BTTC_DIR/keystore

              # init bttc
              ${pkgs.bttc}/bin/bttc --datadir $DATA_DIR init ${bttc-gensis}

              # copy peers file
              if [ ! -f $DATA_DIR/bor/static-nodes.json ]; then
                cp ${bttc-static-nodes} $DATA_DIR/bor/static-nodes.json
              fi

              # if node key not present, create nodekey
              if [ ! -f $NODE_KEY ]; then
                ${pkgs.bttc}/bin/bootnode -genkey $NODE_KEY
              #   # copy node key file
                cp $NODE_KEY $BTTC_DIR/
              fi
            ''
            + (if cfg.deliverySnapShot != null then ''
              ${gnutar}/bin/tar - xzvf ${cfg.bttcSnapShot} -C $BTTC_DIR/data/bor
            '' else ''
              echo "Setup done!"
            '');
            script = ''
              ${pkgs.bttc}/bin/bttc --datadir $DATA_DIR \
              --port 30303 \
              --bor.heimdall "http://localhost:1317" \
              --http --http.addr '127.0.0.1' \
              --http.vhosts '*' \
              --http.corsdomain '*' \
              --http.port 8545 \
              --ipcpath $DATA_DIR/bor.ipc \
              --http.api 'eth,net,web3,txpool,bor' \
              --syncmode 'full' \
              --networkid 1029 \
              --miner.gaslimit '20000000' \
              --miner.gasprice '300000000000000' \
              --miner.gastarget '20000000' \
              --gpo.maxprice '500000000000000' \
              --rpc.allow-unprotected-txs \
              --txpool.nolocals \
              --txpool.accountslots 16 \
              --txpool.globalslots 131072 \
              --txpool.accountqueue 64 \
              --txpool.globalqueue 131072 \
              --txpool.lifetime '1h30m0s' \
              --maxpeers 20 \
              --metrics \
              --pprof --pprof.port 7071 --pprof.addr '0.0.0.0' \
              --unlock $ADDRESS \
              --keystore $BTTC_DIR/keystore \
              --password $BTTC_DIR/password.txt \
              --allow-insecure-unlock \
              --rpc.txfeecap 0 \
              --mine
            '';
          };
          deliveryd = {
            inherit serviceConfig;
            description = "Deliveryd Service Daemon";
            wantedBy = [ "multi-user.target" ];
            after = [ "networking.target" "rabbitmq.service" ];
            before = [ "deliveryd-rest-server.service" ];
            startLimitIntervalSec = 500;
            startLimitBurst = 5;

            environment =
              {
                DELIVERY_HOME_DIR = "${cfg.deliveryHomeDir}";
              };
            preStart = ''
              set -x
              mkdir -p $DELIVERY_HOME_DIR/config
              # init delivery node
              ${pkgs.delivery}/bin/deliveryd init --chain-id delivery-1029 --home $DELIVERY_HOME_DIR
              sed -i '/seeds =/cseeds = \"${delivery-seeds.persistent_peers}\"' $DELIVERY_HOME_DIR/config/config.toml 
              sed -i '/26657/claddr = "tcp://0.0.0.0:26657"' $DELIVERY_HOME_DIR/config/config.toml 
              sed -i '/bttc_rpc_url/cbttc_rpc_url = "${DELIVERY_BTTC_RPC_URL}"' $DELIVERY_HOME_DIR/config/delivery-config.toml 
              sed -i '/eth_rpc_url/ceth_rpc_url = "${DELIVERY_ETH_RPC_URL}"' $DELIVERY_HOME_DIR/config/delivery-config.toml 
              sed -i '/bsc_rpc_url/cbsc_rpc_url = "${BSC_RPC_URL}"' $DELIVERY_HOME_DIR/config/delivery-config.toml 
              sed -i '/tron_rpc_url/ctron_rpc_url = "${TRON_RPC_URL}"' $DELIVERY_HOME_DIR/config/delivery-config.toml 
              sed -i '/tron_grid_url/ctron_grid_url = "${TRON_GRID_URL}"' $DELIVERY_HOME_DIR/config/delivery-config.toml
              sed -i '/tron_grid_api_key/ctron_grid_api_key = "${cfg.tronGridApiKey}"' $DELIVERY_HOME_DIR/config/delivery-config.toml
              # copy node directories to home directories
              cp -rf ${delivery-genesis} $DELIVERY_HOME_DIR/config/delivery_genesis.json
              ${update_toml "prometheus" (if cfg.prometheus then "true" else "false") "$DELIVERY_HOME_DIR/config/config.toml"}
            '' + (if cfg.deliverySnapShot != null then ''
              ${pkgs.gnutar}/bin/tar -xzvf ${cfg.deliverySnapShot} -C $DELIVERY_HOME_DIR/data/
            '' else "");
            script = ''
              ${pkgs.delivery}/bin/deliveryd start --home $DELIVERY_HOME_DIR
            '';

          };
          deliveryd-rest-server = {
            inherit serviceConfig;
            description = "Bttc Service Daemon";
            wantedBy = [ "multi-user.target" ];
            after = [ "networking.target" ];
            before = [ "deliveryd-birdge.service" ];
            startLimitIntervalSec = 500;
            startLimitBurst = 5;
            environment =
              {
                DELIVERY_HOME_DIR = "${cfg.deliveryHomeDir}";
              };
            script = ''
              ${pkgs.delivery}/bin/deliveryd --home $DELIVERY_HOME_DIR --laddr tcp://0.0.0.0:1317 --node http://localhost:26657 rest-server
            '';
          };
          deliveryd-bridge = {
            inherit serviceConfig;
            description = "Bttc Service Daemon";
            wantedBy = [ "multi-user.target" ];
            after = [ "networking.target" "deliveryd.service" ];
            before = [ "deliveryd-birdge.service" ];
            startLimitIntervalSec = 500;
            startLimitBurst = 5;
            environment =
              {
                DELIVERY_HOME_DIR = "${cfg.deliveryHomeDir}";
              };
            script = ''
              ${pkgs.delivery}/bin/bridge --home $DELIVERY_HOME_DIR start --all --node http://localhost:26657 --log_level = debug
            '';
          };
        };
        users.users.bttc = {
          description = "bttc user";
          isSystemUser = true;
          group = "bttc";
        };
        users.groups.bttc = { };
      };
}














