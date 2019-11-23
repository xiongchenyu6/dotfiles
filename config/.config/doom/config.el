;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq user-full-name "XiongChenYu"
      user-mail-address "xiongchenyu@bigo.sg"
      doom-font (font-spec :family "Hack" :size 14)
      doom-modeline-github t
      doom-modeline-major-mode-color-icon t
      )

;; (setq projectile-git-submodule-commanlispyvilled nil)

(setq lsp-message-project-root-warning t)

(set-lookup-handlers! 'emacs-lisp-mode :documentation #'helpful-at-point)

;; (setq evil-move-beyond-eol t)
(set-lookup-handlers! 'emacs-library-link :documentation )

(after! lispy
  (setq lispy-outline "^;; \\(?:;[^#]\\|\\*+\\)"
        lispy-outline-header ";; "
        lispy-ignore-whitespace t)
  (map! :map lispy-mode-map
        :i "C-c (" #'lispy-wrap-round
        :i "_" #'special-lispy-different
        "d" nil
        :i [remap delete-backward-char] #'lispy-delete-backward))

                                        ;(remove-hook 'emacs-lisp-mode-hook #'lispy-mode)

;; Also use lispyville in prog-mode for [ ] < >
(after! lispyville
  ;; (lispyville-set-key-theme
  ;;  '(operators
  ;;    c-w
  ;;    (escape insert)
  ;;    (slurp/barf-lispy)
  ;;    additional-movement))
  (map! :map lispyville-mode-map
        :i "C-w" #'backward-delete-char
        )
  )

(setq magit-repository-directories '(("~/workspace" . 2)))

(after! projectile
  (setq compilation-read-command nil)  ; no prompt in projectile-compile-project
  ;; . -> Build
  (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                    :compile "cmake --build Debug"
                                    :run "./Debug/bin/main"
                                    :test "ctest"
                                    )
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
  )

;; (after! projectile
;;   (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
;;   )

;;mu4e
;; give me ISO(ish) format date-time stamps in the header list
(setq mu4e-attachment-dir "~/Downloads/" mu4e-maildir
      "~/mail/" mu4e-get-mail-command "mbsync -a -q"
      mu4e-update-interval 100 mu4e-view-show-images
      t mu4e-view-prefer-html t mu4e-sent-messages-behavior
      'delete message-kill-buffer-on-exit t mu4e-headers-auto-update
      t org-mu4e-link-query-in-headers-mode nil)


(after! mu4e
  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/bin/msmtp"))

;; (setq message-sendmail-extra-arguments '("--read-envelope-from"))
;; (setq message-sendmail-f-is-evil 't)

;; convert org mode to HTML automatically
(setq org-mu4e-convert-to-html t)

;; (menu-bar-mode 1)

(setq indent-guide-global-mode t)

;; gpg
;; (with-eval-after-load 'mu4e-utils
;; (add-hook 'mu4e-compose-mode-hook 'org-mu4e-compose-org-mode)
;; )

(map!
 ;; Easier window movement
 :n "C-h" 'evil-window-left
 :n "C-j" 'evil-window-down
 :n "C-k" 'evil-window-up
 :n "C-l" 'evil-window-right
 :n "C-q" 'delete-window

 (:map evil-treemacs-state-map
   "C-h" 'evil-window-left
   "C-l" 'evil-window-right)
 )

(setq rmh-elfeed-org-files '("~/Dropbox/Org/elfeed.org"))

(setq org-directory "~/Dropbox/Org"
      org-agenda-files
      (list org-directory)
      org-agenda-diary-file
      (concat org-directory "/diary.org")
      org-default-notes-file
      (concat org-directory "/refile.org"))

(setq org-src-preserve-indentation t)

;; (after! tabbar-mode
;;  ;; Tabbar settings
;;  (set-face-attribute
;;   'tabbar-unselected nil
;;   :background "black"
;;   :foreground "white"
;;   :box '(:line-width 2 :color "gray30" :style nil))
;;  (set-face-attribute
;;   'tabbar-selected nil
;;   :background "gray75"
;;   :foreground "black"
;;   :box '(:line-width 2 :color "gray75" :style nil))
;;  )

(setq
 gdb-many-windows t
 gdb-show-main t)

(setq deft-directory "~/Dropbox/Org")

(evil-define-key 'normal cider-mode-map (kbd "gd") #'cider-find-var)
(evil-define-key 'normal cider-mode-map (kbd "C-o") #'cider-pop-back)

(after! org
  (remove-hook 'org-tab-first-hook #'+org|cycle-only-current-subtree t))

(global-auto-revert-mode)

(setq c-syntactic-indentation nil)

(map!
 :map (org-mode-map)
 :i "<S-return>" #'org-insert-heading
 :i "<C-return>" #'org-insert-subheading)

(after! lsp-mode (setq lsp-ui-doc-use-webkit t
                       lsp-ui-doc-max-height 30
                       lsp-ui-doc-max-width 85
                       ))

;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")

(advice-remove #'org-export-output-file-name #'+org*export-output-file-name)

(require 'org)
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-src-fontify-natively t)

(setq org-latex-compiler "pdflatex --shell-escape %f")

;transparent adjustment
 (set-frame-parameter (selected-frame)'alpha '(95 . 95))
 (add-to-list 'default-frame-alist'(alpha . (95 . 95)))

 (load "~/.config/doom/member-functions.el")
 (require 'member-functions)

 (define-derived-mode prometheus-v2-rules-mode yaml-mode "prometheus rule" ())

 (add-to-list 'auto-mode-alist '("\\.rules$" . prometheus-v2-rules-mode))

 (require 'flycheck)
 (flycheck-define-checker prometheus-v2-promtool-rules
    "A prometheus rules checker using promtool.
  See URL `https://github.com/prometheus/prometheus/tree/master/cmd/promtool'."
    :command ("promtool" "check" "rules" (eval (expand-file-name (buffer-file-name))))
    :standard-input t
    :error-patterns
    ((error (zero-or-more not-newline) "\n"
            (zero-or-more not-newline) "\n"
            (zero-or-more not-newline)
            (zero-or-more "\n")
            " line " line ":" (message)))
    :modes prometheus-v2-rules-mode)

  (add-to-list 'flycheck-checkers 'prometheus-v2-promtool-rules)


(after! cc-mode
  (map!
   :map (c-mode-map c++-mode-map)
   (:localleader
     :n "p" #'ccls-preprocess-file
     :n "r" #'ccls-reload
     :n "h" #'ccls-member-hierarchy
     :n "e" #'expand-member-functions
     :desc "breakpoint"
     :n "db" (lambda ()
               (interactive)
               (evil-open-above 1)
               (insert "volatile static int z=0;while(!z)asm(\"pause\");")
               (evil-normal-state))
     :n "dd" #'realgud:gdb
     ))
  )

(after! haskell-mode
  (map!
    :map haskell-mode-map
        ;; this is set to use cabal for dante users and stack for intero users:
      (:localleader
        (:prefix ("r" . "repl")
          :n "l" #'haskell-process-load-or-reload
          :n "d" #'haskell-process-reload-devel-main )
        )))

(setq centaur-tabs-set-icons t)
(define-key evil-normal-state-map (kbd "g t") 'centaur-tabs-forward)
(define-key evil-normal-state-map (kbd "g T") 'centaur-tabs-backward)

(setq lsp-file-watch-threshold nil)

(dap-mode 1)
(dap-ui-mode 1)
;; enables mouse hover support
(dap-tooltip-mode 1)
;; use tooltips for mouse hover
;; if it is not enabled `dap-mode' will use the minibuffer.
(tooltip-mode 1)
