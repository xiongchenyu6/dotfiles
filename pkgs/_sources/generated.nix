# This file was generated by nvfetcher, please do not modify it manually.
{ fetchgit, fetchurl, fetchFromGitHub }:
{
  alias-tips = {
    pname = "alias-tips";
    version = "cd13ef223c4f310d774cdf8cb0435474cc2bcbbe";
    src = fetchgit {
      url = "https://github.com/djui/alias-tips.git";
      rev = "cd13ef223c4f310d774cdf8cb0435474cc2bcbbe";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-fDYAJSJHatxHhAT68AH4PuIHdwxFXipAoDHITVpkkkE=";
    };
  };
  bttc = {
    pname = "bttc";
    version = "v1.0.2";
    src = fetchFromGitHub ({
      owner = "bttcprotocol";
      repo = "bttc";
      rev = "v1.0.2";
      fetchSubmodules = true;
      sha256 = "sha256-qptNGs6NmbIEzmosASAvdZSIUhw5RVcgEF55/RhgR3I=";
    });
  };
  delivery = {
    pname = "delivery";
    version = "v1.0.2";
    src = fetchFromGitHub ({
      owner = "bttcprotocol";
      repo = "delivery";
      rev = "v1.0.2";
      fetchSubmodules = true;
      sha256 = "sha256-HItF37qlIm6IT8WSomTulkX7524UZwpwMbqyjckXFxA=";
    });
  };
  forgit = {
    pname = "forgit";
    version = "25789d2198f364a8e4a942cf8493fae2ef7b9fe4";
    src = fetchgit {
      url = "https://github.com/wfxr/forgit.git";
      rev = "25789d2198f364a8e4a942cf8493fae2ef7b9fe4";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-ha456LUCUctUn8WAThDza437U5iyUkFirQ2UBtrrROg=";
    };
  };
  launch = {
    pname = "launch";
    version = "f71388e3a8091a1c6bc10aa03d7edf8e7b638f23";
    src = fetchgit {
      url = "https://github.com/bttcprotocol/launch.git";
      rev = "f71388e3a8091a1c6bc10aa03d7edf8e7b638f23";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-5iUjWxjTmMxkfo+WZnjCKmHNTeaKkv5jdiDawMmvbQw=";
    };
  };
  my_cookies = {
    pname = "my_cookies";
    version = "0.1.3";
    src = fetchurl {
      url = "https://pypi.io/packages/source/m/my_cookies/my_cookies-0.1.3.tar.gz";
      sha256 = "sha256-3e5j0HFOXUyUo6YVUKQnbaxvAUtDoRTzGqW8HUfzrQ8=";
    };
  };
  wakatime-zsh-plugin = {
    pname = "wakatime-zsh-plugin";
    version = "69c6028b0c8f72e2afcfa5135b1af29afb49764a";
    src = fetchgit {
      url = "https://github.com/sobolevn/wakatime-zsh-plugin.git";
      rev = "69c6028b0c8f72e2afcfa5135b1af29afb49764a";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-pA1VOkzbHQjmcI2skzB/OP5pXn8CFUz5Ok/GLC6KKXQ=";
    };
  };
}
