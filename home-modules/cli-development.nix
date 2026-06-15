# Full development CLI tier — extends cli-server with IDE-like tools
{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
{
  imports = [
    ./cli-server.nix
  ];

  # Override gpg-agent pinentry for desktop (graphical)
  services.gpg-agent.pinentry.package =
    if pkgs.stdenv.isDarwin then pkgs.pinentry_mac else pkgs.pinentry-gnome3;

  # Desktop gets full neovim with all LSPs
  modules.neovim.lightweight = false;

  programs = {
    lazygit = {
      enable = true;
    };
    lazydocker = {
      enable = true;
    };
    lazysql = {
      enable = true;
    };
    claude-code = {
      enable = true;
      package = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.claude-code;
    };
    antigravity-cli = {
      enable = true;
    };
    codex = {
      enable = true;
    };
    opencode = {
      enable = true;
    };
    k9s = {
      enable = true;
      settings = {
        k9s = {
          readOnly = false;
          logger = {
            textWrap = true;
            showTime = true;
          };
        };
      };
      plugins = {
        cert-status = {
          shortCut = "Shift-S";
          confirm = false;
          description = "Certificate status";
          scopes = [ "certificates" ];
          command = "bash";
          background = false;
          args = [
            "-c"
            "${pkgs.cmctl}/bin/cmctl status certificate --context $CONTEXT -n $NAMESPACE $NAME |& less"
          ];
        };
        cert-renew = {
          shortCut = "Shift-R";
          confirm = false;
          description = "Certificate renew";
          scopes = [ "certificates" ];
          command = "bash";
          background = false;
          args = [
            "-c"
            "${pkgs.cmctl}/bin/cmctl renew --context $CONTEXT -n $NAMESPACE $NAME |& less"
          ];
        };
        secret-inspect = {
          shortCut = "Shift-I";
          confirm = false;
          description = "Inspect secret";
          scopes = [ "secrets" ];
          command = "bash";
          background = false;
          args = [
            "-c"
            "${pkgs.cmctl}/bin/cmctl inspect secret --context $CONTEXT -n $NAMESPACE $NAME |& less"
          ];
        };
        debug = {
          shortCut = "Shift-D";
          description = "Add debug container";
          dangerous = true;
          scopes = [ "containers" ];
          command = "bash";
          background = false;
          confirm = true;
          args = [
            "-c"
            "${pkgs.kubectl}/bin/kubectl --kubeconfig=$KUBECONFIG debug -it --context $CONTEXT -n=$NAMESPACE $POD --target=$NAME --image=nicolaka/netshoot:v0.13 --share-processes -- bash"
          ];
        };
        dive = {
          shortCut = "d";
          confirm = false;
          description = "Dive image";
          scopes = [ "containers" ];
          command = "${pkgs.dive}/bin/dive";
          background = false;
          args = [
            "$COL-IMAGE"
          ];
        };
        toggle-helmrelease = {
          shortCut = "Shift-T";
          confirm = true;
          scopes = [ "helmreleases" ];
          description = "Toggle to suspend or resume a HelmRelease";
          command = "bash";
          background = false;
          args = [
            "-c"
            ''
              suspended=$(${pkgs.kubectl}/bin/kubectl --context $CONTEXT get helmreleases -n $NAMESPACE $NAME -o=custom-columns=TYPE:.spec.suspend | tail -1);
              verb=$([ $suspended = "true" ] && echo "resume" || echo "suspend");
              ${pkgs.flux}/bin/flux
              $verb helmrelease
              --context $CONTEXT
              -n $NAMESPACE $NAME
              | less -K
            ''
          ];
        };
        toggle-kustomization = {
          shortCut = "Shift-T";
          confirm = true;
          scopes = [ "kustomizations" ];
          description = "Toggle to suspend or resume a Kustomization";
          command = "bash";
          background = false;
          args = [
            "-c"
            ''
              suspended=$(${pkgs.kubectl}/bin/kubectl --context $CONTEXT get kustomizations -n $NAMESPACE $NAME -o=custom-columns=TYPE:.spec.suspend | tail -1);
              verb=$([ $suspended = "true" ] && echo "resume" || echo "suspend");
              ${pkgs.flux}/bin/flux
              $verb kustomization
              --context $CONTEXT
              -n $NAMESPACE $NAME
              | less -K
            ''
          ];
        };
        reconcile-git = {
          shortCut = "Shift-R";
          confirm = false;
          description = "Flux reconcile";
          scopes = [ "gitrepositories" ];
          command = "bash";
          background = false;
          args = [
            "-c"
            ''
              ${pkgs.flux}/bin/flux
              reconcile source git
              --context $CONTEXT
              -n $NAMESPACE $NAME
              | less -K
            ''
          ];
        };
        reconcile-hr = {
          shortCut = "Shift-R";
          confirm = false;
          description = "Flux reconcile";
          scopes = [ "helmreleases" ];
          command = "bash";
          background = false;
          args = [
            "-c"
            ''
              ${pkgs.flux}/bin/flux
              reconcile helmrelease
              --context $CONTEXT
              -n $NAMESPACE $NAME
              | less -K
            ''
          ];
        };
        reconcile-helm-repo = {
          shortCut = "Shift-Z";
          description = "Flux reconcile";
          scopes = [ "helmrepositories" ];
          command = "bash";
          background = false;
          confirm = false;
          args = [
            "-c"
            ''
              ${pkgs.flux}/bin/flux
              reconcile source helm
              --context $CONTEXT
              -n $NAMESPACE $NAME
              | less -K
            ''
          ];
        };
        reconcile-oci-repo = {
          shortCut = "Shift-Z";
          description = "Flux reconcile";
          scopes = [ "ocirepositories" ];
          command = "bash";
          background = false;
          confirm = false;
          args = [
            "-c"
            ''
              ${pkgs.flux}/bin/flux
              reconcile source oci
              --context $CONTEXT
              -n $NAMESPACE $NAME
              | less -K
            ''
          ];
        };
        reconcile-ks = {
          shortCut = "Shift-R";
          confirm = false;
          description = "Flux reconcile";
          scopes = [ "kustomizations" ];
          command = "bash";
          background = false;
          args = [
            "-c"
            ''
              ${pkgs.flux}/bin/flux
              reconcile kustomization
              --context $CONTEXT
              -n $NAMESPACE $NAME
              | less -K
            ''
          ];
        };
        reconcile-ir = {
          shortCut = "Shift-R";
          confirm = false;
          description = "Flux reconcile";
          scopes = [ "imagerepositories" ];
          command = "sh";
          background = false;
          args = [
            "-c"
            ''
              ${pkgs.flux}/bin/flux
              reconcile image repository
              --context $CONTEXT
              -n $NAMESPACE $NAME
              | less -K
            ''
          ];
        };
        reconcile-iua = {
          shortCut = "Shift-I";
          confirm = false;
          description = "Flux reconcile";
          scopes = [ "imageupdateautomations" ];
          command = "sh";
          background = false;
          args = [
            "-c"
            ''
              ${pkgs.flux}/bin/flux
              reconcile image update
              --context $CONTEXT
              -n $NAMESPACE $NAME
              | less -K
            ''
          ];
        };
        get-suspended-helmreleases = {
          shortCut = "Shift-S";
          confirm = false;
          description = "Suspended Helm Releases";
          scopes = [ "helmrelease" ];
          command = "sh";
          background = false;
          args = [
            "-c"
            ''
              ${pkgs.kubectl}/bin/kubectl get
              --context $CONTEXT
              --all-namespaces
              helmreleases.helm.toolkit.fluxcd.io -o json
              | ${pkgs.jq}/bin/jq -r '.items[] | select(.spec.suspend==true) | [.metadata.namespace,.metadata.name,.spec.suspend] | @tsv'
              | less -K
            ''
          ];
        };
        get-suspended-kustomizations = {
          shortCut = "Shift-S";
          confirm = false;
          description = "Suspended Kustomizations";
          scopes = [ "kustomizations" ];
          command = "sh";
          background = false;
          args = [
            "-c"
            ''
              ${pkgs.kubectl}/bin/kubectl get
              --context $CONTEXT
              --all-namespaces
              kustomizations.kustomize.toolkit.fluxcd.io -o json
              | ${pkgs.jq}/bin/jq -r '.items[] | select(.spec.suspend==true) | [.metadata.name,.spec.suspend] | @tsv'
              | less -K
            ''
          ];
        };
        helm-default-values = {
          shortCut = "Shift-V";
          confirm = false;
          description = "Chart Default Values";
          scopes = [ "helm" ];
          command = "sh";
          background = false;
          args = [
            "-c"
            ''
              revision=$(${pkgs.kubernetes-helm}/bin/helm history -n $NAMESPACE --kube-context $CONTEXT $COL-NAME | grep deployed | cut -d$'\t' -f1 | tr -d ' \t');
              ${pkgs.kubectl}/bin/kubectl \
              get secrets \
              --context $CONTEXT \
              -n $NAMESPACE \
              sh.helm.release.v1.$COL-NAME.v$revision -o yaml \
              | ${pkgs.yq-go}/bin/yq e '.data.release' - \
              | ${pkgs.coreutils}/bin/base64 -d \
              | ${pkgs.coreutils}/bin/base64 -d \
              | ${pkgs.gzip}/bin/gunzip \
              | ${pkgs.jq}/bin/jq -r '.chart.values' \
              | ${pkgs.yq-go}/bin/yq -P \
              | less -K
            ''
          ];
        };
        helm-diff-current = {
          shortCut = "Shift-Q";
          confirm = false;
          description = "Diff with Current Revision";
          scopes = [ "history" ];
          command = "bash";
          background = false;
          args = [
            "-c"
            ''
              RELEASE_NAME=$(echo $NAME | cut -d':' -f1);
              LATEST_REVISION=$(${pkgs.kubernetes-helm}/bin/helm history -n $NAMESPACE --kube-context $CONTEXT $RELEASE_NAME | grep deployed | cut -d$'\t' -f1 | tr -d ' \t');
              ${pkgs.kubernetes-helm}/bin/helm diff revision $RELEASE_NAME $LATEST_REVISION $COL-REVISION --kube-context $CONTEXT --namespace $NAMESPACE --color | less -RK
            ''
          ];
        };
        helm-values = {
          shortCut = "v";
          confirm = false;
          description = "Values";
          scopes = [ "helm" ];
          command = "sh";
          background = false;
          args = [
            "-c"
            "${pkgs.kubernetes-helm}/bin/helm get values $COL-NAME -n $NAMESPACE --kube-context $CONTEXT | less -K"
          ];
        };
        toggleCronjob = {
          shortCut = "Ctrl-S";
          confirm = true;
          dangerous = true;
          scopes = [ "cj" ];
          description = "Toggle to suspend or resume a running cronjob";
          command = "${pkgs.kubectl}/bin/kubectl";
          background = true;
          args = [
            "patch"
            "cronjobs"
            "$NAME"
            "-n"
            "$NAMESPACE"
            "--context"
            "$CONTEXT"
            "-p"
            ''{"spec" : {"suspend" : $!COL-SUSPEND }}''
          ];
        };
        secret-openssl-ca = {
          shortCut = "Ctrl-O";
          confirm = false;
          description = "Openssl ca.crt";
          scopes = [ "secrets" ];
          command = "bash";
          background = false;
          args = [
            "-c"
            "${pkgs.kubectl}/bin/kubectl get secret --context $CONTEXT -n $NAMESPACE $NAME -o jsonpath='{.data.ca\.crt}' | ${pkgs.coreutils}/bin/base64 -d | ${pkgs.openssl}/bin/openssl storeutl -noout -text -certs /dev/stdin |& less"
          ];
        };
        secret-openssl-tls = {
          shortCut = "Shift-O";
          confirm = false;
          description = "Openssl tls.crt";
          scopes = [ "secrets" ];
          command = "bash";
          background = false;
          args = [
            "-c"
            "${pkgs.kubectl}/bin/kubectl get secret --context $CONTEXT -n $NAMESPACE $NAME -o jsonpath='{.data.tls\.crt}' | ${pkgs.coreutils}/bin/base64 -d | ${pkgs.openssl}/bin/openssl storeutl -noout -text -certs /dev/stdin |& less"
          ];
        };
        remove_finalizers = {
          shortCut = "Ctrl-F";
          confirm = true;
          dangerous = true;
          scopes = [ "all" ];
          description = ''
            Removes all finalizers from selected resource. Be careful when using it,
            it may leave dangling resources or delete them
          '';
          command = "${pkgs.kubectl}/bin/kubectl";
          background = true;
          args = [
            "patch"
            "--context"
            "$CONTEXT"
            "--namespace"
            "$NAMESPACE"
            "$RESOURCE_NAME.$RESOURCE_GROUP"
            "$NAME"
            "-p"
            ''{"metadata":{"finalizers":null}}''
            "--type"
            "merge"
          ];
        };
        rm-ns = {
          shortCut = "n";
          confirm = true;
          dangerous = true;
          description = "Remove NS Finalizers";
          scopes = [ "namespace" ];
          command = "sh";
          background = false;
          args = [
            "-c"
            "${pkgs.kubectl}/bin/kubectl get namespace $NAME -o json | ${pkgs.jq}/bin/jq '.spec.finalizers=[]' | ${pkgs.kubectl}/bin/kubectl replace --raw /api/v1/namespaces/$NAME/finalize -f - > /dev/null"
          ];
        };
        trace-dns = {
          shortCut = "Shift-L";
          description = "Trace DNS requests";
          scopes = [
            "containers"
            "pods"
            "nodes"
          ];
          command = "bash";
          confirm = false;
          background = false;
          args = [
            "-c"
            ''
              IG_VERSION=v0.34.0
              IG_IMAGE=ghcr.io/inspektor-gadget/ig:$IG_VERSION
              IG_FIELD=k8s.podName,src,dst,qr,qtype,name,rcode,latency_ns

              GREEN='\033[0;32m'
              RED='\033[0;31m'
              BLUE='\033[0;34m'
              NC='\033[0m' # No Color

              # Ensure kubectl version is 1.30 or later
              KUBECTL_VERSION=$(${pkgs.kubectl}/bin/kubectl version --client | awk '/Client Version:/{print $3}')
              if [[ "$(echo "$KUBECTL_VERSION" | cut -d. -f2)" -lt 30 ]]; then
                echo -e "''${RED}kubectl version 1.30 or later is required''${NC}"
                sleep 3
                exit
              fi

              clear

              # Handle containers
              if [[ -n "$POD" ]]; then
                echo -e "''${GREEN}Tracing DNS requests for container ''${BLUE}''${NAME}''${GREEN} in pod ''${BLUE}''${POD}''${GREEN} in namespace ''${BLUE}''${NAMESPACE}''${NC}"
                IG_NODE=$(${pkgs.kubectl}/bin/kubectl get pod "$POD" -n "$NAMESPACE" -o jsonpath='{.spec.nodeName}')
                ${pkgs.kubectl}/bin/kubectl debug --kubeconfig=$KUBECONFIG  --context=$CONTEXT -q \
                  --profile=sysadmin "node/$IG_NODE" -it --image="$IG_IMAGE" -- \
                  ig run trace_dns:$IG_VERSION -F "k8s.podName==$POD" -F "k8s.containerName=$NAME" \
                  --fields "$IG_FIELD"
                  exit
              fi

              # Handle pods
              if [[ -n "$NAMESPACE" ]]; then
                echo -e "''${GREEN}Tracing DNS requests for pod ''${BLUE}''${NAME}''${GREEN} in namespace ''${BLUE}''${NAMESPACE}''${NC}"
                IG_NODE=$(${pkgs.kubectl}/bin/kubectl get pod "$NAME" -n "$NAMESPACE" -o jsonpath='{.spec.nodeName}')
                ${pkgs.kubectl}/bin/kubectl debug --kubeconfig=$KUBECONFIG  --context=$CONTEXT -q \
                  --profile=sysadmin  -it --image="$IG_IMAGE" "node/$IG_NODE" -- \
                  ig run trace_dns:$IG_VERSION -F "k8s.podName==$NAME" \
                  --fields "$IG_FIELD"
                  exit
              fi

              # Handle nodes
              echo -e "''${GREEN}Tracing DNS requests for node ''${BLUE}''${NAME}''${NC}"
              ${pkgs.kubectl}/bin/kubectl debug --kubeconfig=$KUBECONFIG  --context=$CONTEXT -q \
                --profile=sysadmin -it --image="$IG_IMAGE" "node/$NAME" -- \
                ig run trace_dns:$IG_VERSION --fields "$IG_FIELD"
            ''
          ];
        };
        watch-events = {
          shortCut = "Shift-E";
          confirm = false;
          description = "Get Events";
          scopes = [ "all" ];
          command = "sh";
          background = false;
          args = [
            "-c"
            "${pkgs.kubectl}/bin/kubectl events --context $CONTEXT --namespace $NAMESPACE --for $RESOURCE_NAME.$RESOURCE_GROUP/$NAME --watch"
          ];
        };
      };
    };

    git-cliff = {
      enable = true;
    };

    pandoc = {
      enable = true;
    };

    sqls = {
      enable = true;
    };

    broot = {
      enable = true;
      settings = { };
    };

    java = {
      enable = true;
    };

    topgrade = {
      # Override server topgrade with desktop settings
      settings = {
        misc = {
          only = lib.mkForce [
            "system"
            "git_repos"
            "tldr"
          ];
        };
        git = {
          max_concurrency = 10;
          repos = [
            "~/workspace/*/"
            "~/git/*/"
            "~/private/*/"
          ];
          arguments = "--rebase --autostash";
        };
        commands = { };
      };
    };

    yt-dlp = {
      enable = true;
    };

    aria2 = {
      enable = true;
      settings = {
        enable-rpc = true;
        rpc-allow-origin-all = true;
        rpc-listen-all = true;
        max-concurrent-downloads = 5;
        continue = true;
        max-connection-per-server = 5;
        min-split-size = "10 M";
        split = 10;
        max-overall-download-limit = 0;
        max-download-limit = 0;
        max-overall-upload-limit = 0;
        max-upload-limit = 0;
        dir = "${config.home.homeDirectory}/Downloads";
        enable-mmap = true;
        file-allocation = "prealloc";
      };
    };

    gh-dash = {
      enable = true;
    };

    sbt = {
      enable = true;
      plugins = [
        {
          artifact = "sbt-updates";
          org = "com.timushev.sbt";
          version = "latest.integration";
        }
        {
          artifact = "sbt-stats";
          org = "com.orrsella";
          version = "latest.integration";
        }
      ];
    };
    go = {
      enable = true;
    };
    yazi = {
      enable = true;
    };

    # Rustup and grafana-loki completions (moved from zsh.nix)
    zsh.initContent = ''
      eval "$(${pkgs.rustup}/bin/rustup completions zsh)"
      eval "$(${pkgs.grafana-loki}/bin/logcli --completion-script-zsh)"
    '';
  };

  # Electron flags config files — Linux/Wayland only
  home.file = lib.mkIf pkgs.stdenv.isLinux {
    ".config/electron-flags.conf" = {
      text = ''
        --enable-wayland-ime
        --enable-features=WaylandWindowDecorations
        --ozone-platform-hint=auto        '';
    };

    ".config/electron25-flags.conf" = {
      text = ''
        --enable-wayland-ime
        --enable-features=WaylandWindowDecorations
        --ozone-platform-hint=auto        '';
    };
    ".config/electron24-flags.conf" = {
      text = ''
        --enable-wayland-ime
        --enable-features=UseOzonePlatform
        --ozone-platform=wayland
      '';
    };
    ".config/electron23-flags.conf" = {
      text = ''
        --enable-wayland-ime
        --enable-features=UseOzonePlatform
        --ozone-platform=wayland
      '';
    };
    ".config/electron22-flags.conf" = {
      text = ''
        --enable-wayland-ime
        --enable-features=UseOzonePlatform
        --ozone-platform=wayland
      '';
    };
  };
}
