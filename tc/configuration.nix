{ ... }: {
  imports = [
    ./hardware-configuration.nix
    
    
  ];

  boot.cleanTmpDir = true;
  zramSwap.enable = true;
  networking.hostName = "VM-8-10-ubuntu";
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCzzKV5IF7ekz9oJQ37nbaUNhXKkQ4KzJiDOYVRVroFq+LEJZHqNxe/Lt1Z1cKvFjRruu6f3clzqRargKlmqbO1d8mJZy0R9TbKQxleEZZq2cZJemX99xrkiu9keBF2qhohwn28v0JUuUyjNo188/YyS1tocoWFNZtp7qPiK8HRF7LQQ99nOa3zGmZJQL5Rvs2RFTFMGhiehsq8aXFuTZNejjivl5BFJjzxoVqZSbB8//lwsGZWpU5Ue54KV51UTv+9wDh2myuyenP/ZbdK9UZo9abCIeI52F9QbGJtjz6cOKG6oz67x06EYxvD/HKJ/uPuisy/cu+rPInmaF5AZTnd skey-p1r300u3" 
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC3LHhrdC+Mor6lM9U0fxyJ2WCn4CzNUZPyOP8ACQpAl5bADYY8ici2SbRD6y0dZnwNvSUJKw090HXiPOgKCYrfPQPX4IOgiPLqPBJq0JCI7w7/pewRmg1bd/5k5BC8C5x0P2H63DovDXEnyIJxqZnVWZDjfhysGEVGueoYBxeDAHHBZLwGGxW36oX8OmfiTGDmMrHWqQxKpluR6KIbe4aFML+ZIol0Vy6+244gREZZXn6xTAoCxRGghaEnOf5X3SivKOJHLTDpAXI7JYesepHHyCPd+OXH2VzSVj0qqOtzb5t6mNHkM4wC9HhTqPT/KWuxjv9HcpXjag9ZGuby/LxOo+6knb5a7VtRm0GxvbBptNS5Frlrl9HNwARiqSmiaSvOWydrZYKV0/ClIYdA7f4DMUc46KIP+wHqLXO2oBe5I4sK4TesmOxCYezi2ti/T4sC/e4Hlvgc/luvS6p0GdTtZ0wQLMmqz2u79LVRpjQMFygLa0IQXFo7c+0FqB7Et8M= skey-21jfw3n7" 
  ];
}
