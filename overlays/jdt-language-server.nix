_: prev: {
  jdt-language-server = prev.jdt-language-server.overrideAttrs (_: {
    installPhase = let
      # The application ships with config directories for linux and mac
      configDir = if prev.stdenv.isDarwin then "config_mac" else "config_linux";
    in ''
      install -D -t $out/share/java/plugins/ plugins/*.jar
      install -Dm 444 -t $out/share/config ${configDir}/*
      launcher="$(ls $out/share/java/plugins/org.eclipse.equinox.launcher_* | sort -V | tail -n1)"
      makeWrapper ${prev.jdk}/bin/java $out/bin/jdtls \
      --add-flags "-Declipse.application=org.eclipse.jdt.ls.core.id1" \
      --add-flags "-Dosgi.bundles.defaultStartLevel=4" \
      --add-flags "-Declipse.product=org.eclipse.jdt.ls.core.product" \
      --add-flags "-Dosgi.sharedConfiguration.area=$out/share/config" \
      --add-flags "-Dosgi.sharedConfiguration.area.readOnly=true" \
      --add-flags "-Dosgi.checkConfiguration=true" \
      --add-flags "-Dosgi.configuration.cascaded=true" \
      --add-flags "-Dlog.level=ALL" \
      --add-flags "-noverify" \
      --add-flags "\$JAVA_OPTS" \
      --add-flags "-jar $launcher" \
      --add-flags "--add-modules=ALL-SYSTEM" \
      --add-flags "--add-opens java.base/java.util=ALL-UNNAMED" \
      --add-flags "--add-opens java.base/java.lang=ALL-UNNAMED"
    '';
  });
  emacsPackages.rime = prev.emacsPackages.rime.overrideAttrs (old:
    with prev; {
      buildInputs = [ librime emacsPgtk ];
      nativeBuildInputs = [ gnumake pkg-config tree ];
      postInstall = ''
        cd source
        make lib
        cp librime-emacs.so $out/share/emacs/site-lisp/elpa/rime-${old.version}/
      '';
    });
}
