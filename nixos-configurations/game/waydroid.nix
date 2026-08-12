{ config, lib, ... }:
{
  # Refresh hardware-derived Android properties before each container start.
  # Waydroid falls back to SwiftShader for NVIDIA, while stale properties can
  # incorrectly retain Mesa's Radeon Vulkan driver after a GPU configuration
  # change and leave Android stuck before SurfaceFlinger starts.
  systemd.services.waydroid-container.preStart = lib.mkBefore ''
    if [[ -f /var/lib/waydroid/waydroid.cfg ]]; then
      ${config.virtualisation.waydroid.package}/bin/waydroid upgrade --offline
    fi
  '';
}
