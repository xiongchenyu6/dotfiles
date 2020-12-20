;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
(package! protobuf-mode)
(package! company-tabnine)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)
(package! doom-snippets :ignore t)
;; If you want to replace it with yasnippet's default snippets
(package! yasnippet-snippets)

(package! ox-confluence-en :recipe (:host github :repo "correl/ox-confluence-en"))
(package! ccls :disable t)
(package! systemd)

(package! wakatime-mode)
