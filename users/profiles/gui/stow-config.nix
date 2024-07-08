_: {
  home = {
    persistence."/home/freeman.xiong/dotfiles/stow-managed/" = {
      removePrefixDirectory = true;
      allowOther = false;
      directories = [
        "config/.config/xmonad"
        "config/.config/nvim"
        "config/.config/albert"
        "password-store/.local/share/password-store"
        "albert/.local/share/albert"
        "config/.config/emacs"
        "config/.config/Code"
        "rime/.local/share/fcitx5/rime"
        # {
        #   directory = ;
        #   method = "symlink";
        # }
      ];
      files = [ "auth/.authinfo.gpg" ];
    };
  };
}
