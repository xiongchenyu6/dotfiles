{
  config,
  ...
}:
{
  # happier daemon 要把 OPENAI_API_KEY 传给它 fork 出来的 codex 会话 ——
  # happier **不会**自己去读 ~/.codex/auth.json,不给的话会话起得来、手机上
  # 也看得到,但一发消息就没反应。
  #
  # daemon 是 user service(跑在 freeman.xiong 下),所以这份渲染出来的 env
  # 必须让该用户读得到,不能用默认的 root:root 0400。
  sops.secrets."api-keys/OPENAI_API_KEY" = {
    owner = "freeman.xiong";
    mode = "0400";
  };

  sops.templates."happier.env" = {
    owner = "freeman.xiong";
    mode = "0400";
    content = ''
      OPENAI_API_KEY=${config.sops.placeholder."api-keys/OPENAI_API_KEY"}
    '';
  };

  services.happier = {
    enable = true;
    users = [ "freeman.xiong" ];
    environmentFile = config.sops.templates."happier.env".path;

    # daemon 是后台服务,**没有 TTY**,所以只能用 mcp。
    #
    # 三个后端实测下来:
    #   mcp       无需 TTY,可后台常驻;但不支持 local control ——
    #             `localControlBackend` 只认 acp / appServer(见 CLI 的
    #             runCodex),所以 mcp 会话切回本地是空的、没有上下文。
    #   appServer 需要 TTY;支持 local control;但 codex 0.149 在 app-server
    #             模式下只把会话写进 sqlite、不产生 rollout-*.jsonl,而 CLI
    #             在等那个文件,会一直卡在 "Codex rollout file not found yet"。
    #   acp       需要 TTY;支持 local control;起 codex 的参数实测正确
    #             (--ask-for-approval never)。会按需从 GitHub 下载
    #             zed-industries/codex-acp 到 ~/.happier/tools —— 那个二进制
    #             的 interpreter 是 /lib64/ld-linux-x86-64.so.2,靠本机的
    #             nix-ld 才跑得起来。
    #
    # 无 TTY 时 appServer / acp 都只是进程起来、不产生子进程也不写会话日志,
    # 等于废的,所以 daemon 这边没得选。
    extraEnvironment.HAPPIER_CODEX_BACKEND_MODE = "mcp";
  };

  # 终端里手动起的会话有 TTY,用 acp。
  #
  # 不给这个变量的话后端落到 mcp,而 CLI 的 decideCodexLocalControlSupport 里:
  #   localControlBackend = (backend === "acp" || backend === "appServer") ? backend : null
  #   if (!localControlBackend) return { ok: false, reason: "resume-disabled" }
  # 也就是 mcp 会话根本不允许切换控制模式 —— App 上表现为
  # "Failed to switch control mode"。
  #
  # 只给终端不给上面的 daemon:daemon 无 TTY,acp/appServer 在无 TTY 下只会
  # 进程起来、不产生 codex 子进程,等于废的;而 daemon 起的会话本来就被
  # `startedBy === "daemon" && !hasTtyForLocal` 挡在 local control 之外,
  # 给它 acp 没有任何好处。
  #
  # 用 home-manager 的 sessionVariables 而不是 environment.sessionVariables:
  # 后者写进 /etc/set-environment,要重新登录才加载,新开终端标签页拿不到。
  home-manager.users."freeman.xiong".home.sessionVariables.HAPPIER_CODEX_BACKEND_MODE = "acp";
}
