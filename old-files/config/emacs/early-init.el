;; Disable GC during initialization(for the case, early-init.el is not used)
(setq gc-cons-threshold most-positive-fixnum)

;; Ensure we have correct user-emacs-directory
;; The folder of meomacs can be placed anywhere, and started with
;;   emacs -q -l /path/to/meomacs/init.el
(setq user-emacs-directory (file-name-directory (or load-file-name buffer-file-name)))

;; Setup straight as package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Straight configs
(setq straight-vc-git-default-clone-depth 1)

;; Use GCMH as GC config.
(straight-use-package 'gcmh)
(require 'gcmh)
(gcmh-mode 1)

(straight-use-package 'material-theme)

(require 'material-theme)

(setq custom-safe-themes t)
;(load-theme 'material t)
(load-theme 'modus-vivendi t)
