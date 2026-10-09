{
  lib,
  stdenv,
  fetchurl,
  dpkg,
  autoPatchelfHook,
  makeWrapper,
  gtk3,
  glib,
  libpulseaudio,
  libxkbcommon,
  libx11,
  libxfixes,
  libxtst,
  libxcb,
  gst_all_1,
  dbus,
  zlib,
  wayland,
  libdrm,
  libglvnd,
  libva,
  libayatana-appindicator,
  xdotool,
  pipewire,
  alsa-lib,
  systemd,
  curl,
  procps,
  coreutils,
  shadow,
  patchelf,
}:
stdenv.mkDerivation {
  pname = "rustdesk-unattended-wayland";
  version = "1.5.0";

  src = fetchurl {
    url = "https://github.com/rustdesk/rustdesk/releases/download/nightly/rustdesk-unattended-wayland-1.5.0-x86_64.deb";
    hash = "sha256-crZErjNO0OpVrFZlGFZ9sQkiNVolBtaqAYc4jibZv+4=";
  };

  nativeBuildInputs = [
    dpkg
    autoPatchelfHook
    makeWrapper
  ];
  buildInputs = [
    gtk3
    glib
    libpulseaudio
    libxkbcommon
    libx11
    libxfixes
    libxtst
    libxcb
    gst_all_1.gstreamer
    gst_all_1.gst-plugins-base
    dbus
    zlib
    wayland
    libdrm
    stdenv.cc.cc.lib
  ];
  runtimeDependencies = [
    libglvnd
    libva
    libayatana-appindicator
    xdotool
    pipewire
    alsa-lib
    systemd
  ];

  unpackPhase = ''
    runHook preUnpack
    dpkg-deb -x "$src" .
    runHook postUnpack
  '';
  dontConfigure = true;
  dontBuild = true;
  # --service re-executes the ELF through sudo, bypassing the shell wrapper.
  # dlopen calls originate in librustdesk.so, so its own RUNPATH must include
  # the private DRM helper and optional runtime libraries.
  preFixup = ''
    fixRustdeskRunpath() {
      ${patchelf}/bin/patchelf --add-rpath "$out/lib:/run/opengl-driver/lib:${
        lib.makeLibraryPath [
          libglvnd
          libva
          libayatana-appindicator
          pipewire
          alsa-lib
          systemd
        ]
      }" "$out/share/rustdesk/lib/librustdesk.so"
    }
    # Run after autoPatchelf; never rewrite Flutter's libapp.so snapshot.
    postFixupHooks+=(fixRustdeskRunpath)
  '';
  installPhase = ''
    runHook preInstall
    mkdir -p "$out/bin" "$out/share" "$out/lib"
    cp -r usr/share/rustdesk "$out/share/"
    cp -r usr/lib/rustdesk/. "$out/lib/"
    for dir in applications icons; do
      if [ -d "usr/share/$dir" ]; then
        cp -r "usr/share/$dir" "$out/share/"
      fi
    done
    makeWrapper "$out/share/rustdesk/rustdesk" "$out/bin/rustdesk" \
      --prefix LD_LIBRARY_PATH : "$out/lib:/run/opengl-driver/lib" \
      --prefix GST_PLUGIN_SYSTEM_PATH_1_0 : "${gst_all_1.gstreamer}/lib/gstreamer-1.0:${pipewire}/lib/gstreamer-1.0:${gst_all_1.gst-plugins-base}/lib/gstreamer-1.0" \
      --prefix PATH : ${
        lib.makeBinPath [
          curl
          procps
          coreutils
          systemd
          shadow
        ]
      }
    runHook postInstall
  '';

  meta = {
    description = "RustDesk upstream preview with unattended Wayland capture";
    homepage = "https://rustdesk.com/blog/unattended-remote-access-wayland/";
    license = lib.licenses.agpl3Only;
    platforms = [ "x86_64-linux" ];
    mainProgram = "rustdesk";
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
