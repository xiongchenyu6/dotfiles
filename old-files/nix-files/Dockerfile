FROM nixos/nix
RUN nix-channel --add https://nixos.org/channels/nixos-18.09 nixos
RUN nix-channel --update
RUN nix-env -i stack
COPY cryptobox-c.nix /cryptobox-c.nix
COPY ./wire-server /wire-server
RUN nix-build /cryptobox-c.nix
RUN nix-env -f /cryptobox-c.nix -i
