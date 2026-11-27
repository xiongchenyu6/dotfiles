# Remote server self-healing kernel sysctls: auto-reboot on panic, oops,
# soft lockup, or hung task while keeping single-process OOM kills non-fatal.
{ lib, ... }:
{
  boot.kernel.sysctl = {
    "kernel.panic" = lib.mkDefault 10;
    "kernel.panic_on_oops" = lib.mkDefault 1;
    "kernel.softlockup_panic" = lib.mkDefault 1;
    "kernel.hung_task_panic" = lib.mkDefault 1;
    "kernel.hung_task_timeout_secs" = lib.mkDefault 120;
    "vm.panic_on_oom" = lib.mkDefault 0;
  };
}
