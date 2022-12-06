_: {
  programs = {
    git = {
      signing = {
        key = "5AF7AFBF695E8A5D";
        signByDefault = true;
      };
      extraConfig = {
        push = {default = "current";};
        color = {ui = "auto";};
        core = {
          autocrlf = "input";
          editor = "emacs";
        };
        pull = {rebase = false;};
        user = {
          name = "freeman";
          email = "xiongchenyu6@gmail.com";
        };
      };
    };
  };
}
