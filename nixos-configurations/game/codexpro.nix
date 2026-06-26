{
  config,
  ...
}:
let
  # Named Cloudflare tunnel created for this host (cloudflared tunnel create
  # codexpro-game). Stable hostname -> never changes across restarts.
  tunnelId = "fb29b430-f3ca-48bf-a35e-f9d75c496792";
  hostName = "codexpro.panda.qzz.io";
in
{
  # Secrets live in secrets/common.yaml (defaultSopsFile), encrypted to the
  # game host age key. Both are read by systemd via LoadCredential (root), so
  # the default root:root 0400 ownership is correct.
  sops.secrets = {
    "cloudflared/codexpro-game-credentials".restartUnits = [ "cloudflared-tunnel-${tunnelId}.service" ];
    "codexpro/http-token".restartUnits = [ "codexpro.service" ];
  };

  # codexpro MCP server: lets ChatGPT (Developer Mode) read/edit local repos.
  # Bound to loopback; only the tunnel exposes it. One instance serves every
  # project under allowedRoots.
  services.codexpro = {
    enable = true;
    user = "freeman.xiong";
    group = "users";
    root = "/home/freeman.xiong/Documents";
    allowedRoots = [ "/home/freeman.xiong/Documents" ];
    httpTokenFile = config.sops.secrets."codexpro/http-token".path;
    # Maximum permissions: edit any file in the workspace, run any shell command,
    # full tool catalog. Scoped to all of ~/Documents.
    writeMode = "workspace";
    bashMode = "full";
    toolMode = "full";
    # host 127.0.0.1, port 8787 by default — only cloudflared connects locally.
  };

  # Cloudflare named tunnel: public HTTPS at https://codexpro.panda.qzz.io ->
  # the loopback codexpro server. Credentials come from sops.
  services.cloudflared = {
    enable = true;
    tunnels.${tunnelId} = {
      credentialsFile = config.sops.secrets."cloudflared/codexpro-game-credentials".path;
      default = "http_status:404";
      ingress.${hostName} = "http://127.0.0.1:8787";
    };
  };
}
