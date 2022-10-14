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

(setq custom-safe-themes t)


(straight-use-package 'doom-themes)

(load-theme 'doom-monokai-classic t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)
;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)
;; or for treemacs users
(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; (straight-use-package 'material-theme)

;; (load-theme 'material t)



