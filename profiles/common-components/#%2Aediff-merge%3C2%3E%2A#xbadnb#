{ profiles, config, ... }: {
  sops.secrets."oath/seed" = { };
  security = {
    audit = {
      enable = true;
      rules = [
        "-w /var/log/audit/ -k auditlog"
        "-w /etc/audit/ -p wa -k auditconfig"
        "-w /etc/libaudit.conf -p wa -k auditconfig"
        "-w /etc/audisp/ -p wa -k audispconfig"
        "-a always,exclude -F msgtype=AVC"
        "-a always,exclude -F msgtype=CWD"
        "-a always,exclude -F msgtype=EOE"
        "-a always,exclude -F msgtype=CRYPTO_KEY_USER"
        "-a exit,never -F arch=b32 -F dir=/dev/shm -k sharedmemaccess"
        "-a exit,never -F arch=b64 -F dir=/dev/shm -k sharedmemaccess"
        "-w /etc/sysctl.conf -p wa -k sysctl"
        "-a always,exit -F arch=b64 -S finit_module -S init_module -S delete_module -F auid!=-1 -k modules"
        "-a always,exit -F arch=b32 -S finit_module -S init_module -S delete_module -F auid!=-1 -k modules"
        "-w /etc/modprobe.conf -p wa -k modprobe"
        "-a always,exit -F arch=b64 -S kexec_load -k KEXEC"
        "-a always,exit -F arch=b32 -S sys_kexec_load -k KEXEC"
        "-a always,exit -F arch=b64 -S open,creat -F exit=-EACCES -k access"
        "-a always,exit -F arch=b64 -S open,creat -F exit=-EPERM -k access"
        "-a always,exit -F arch=b32 -S open,creat -F exit=-EACCES -k access"
        "-a always,exit -F arch=b32 -S open,creat -F exit=-EPERM -k access"
        "-a always,exit -F arch=b64 -S openat -F exit=-EACCES -k access"
        "-a always,exit -F arch=b64 -S openat -F exit=-EPERM -k access"
        "-a always,exit -F arch=b32 -S openat -F exit=-EACCES -k access"
        "-a always,exit -F arch=b32 -S openat -F exit=-EPERM -k access"
        "-a always,exit -F arch=b64 -S open_by_handle_at -F exit=-EACCES -k access"
        "-a always,exit -F arch=b64 -S open_by_handle_at -F exit=-EPERM -k access"
        "-a always,exit -F arch=b32 -S open_by_handle_at -F exit=-EACCES -k access"
        "-a always,exit -F arch=b32 -S open_by_handle_at -F exit=-EPERM -k access"
      ];
    };
    auditd.enable = true;
    rtkit = { enable = true; };
    sudo = {
      enable = true;
      # wheelNeedsPassword = false;
      # package = pkgs.sudo.override {
      #   withInsults = true;
      #   withSssd = true;
      # };
    };
    acme = { acceptTerms = true; };
    polkit.enable = true;
    pam = {
      krb5.enable = false;
      oath = {
        enable = false;
        usersFile = config.sops.secrets."oath/seed".path;
        window = 30;
      };
    };
    pki = { certificates = map (x: x.cert) profiles.share.root-cas; };
  };
}
