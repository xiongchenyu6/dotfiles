;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq user-full-name "XiongChenYu"
      user-mail-address "xiongchenyu@bigo.sg"
      doom-font (font-spec :family
                           "JetBrains Mono" :size 15
                           ;; "Hasklig" :size 14
                            ;; "Fira Code" :size 14
                           )
      doom-modeline-github t
      doom-modeline-major-mode-color-icon t
      )
(setq lsp-message-project-root-warning t)

(set-lookup-handlers! 'emacs-lisp-mode :documentation #'helpful-at-point)

;; (setq evil-move-beyond-eol t)
(set-lookup-handlers! 'emacs-library-link :documentation )

(after! lispy
  (setq lispy-outline "^;; \\(?:;[^#]\\|\\*+\\)"
        lispy-outline-header ";; "
        lispy-ignore-whitespace t)
  (map! :map lispy-mode-map
        :i "M-)" #'lispy-parens-auto-wrap
        :i "M-}" #'lispy-braces-auto-wrap
        :i "M-]" #'lispy-brackets-auto-wrap
        :i "_" #'special-lispy-different
        :i [remap delete-backward-char] #'lispy-delete-backward)
  )




(setq magit-repository-directories '(("~/workspace" . 2)))

(setq compilation-read-command nil) ; no prompt in projectile-compile-project

(after! projectile
  ;; . -> Build
  (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                    :compile "cmake --build Debug"
                                    :run "./Debug/main"
                                    :test "ctest"
                                    ))

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

(setq rmh-elfeed-org-files '("~/Dropbox/Org/fun/elfeed.org"))

(setq org-roam-directory  "~/Dropbox/Org/")

(setq org-directory "~/Dropbox/Org"
      org-agenda-files
      (list org-directory)
      org-agenda-diary-file
      (concat org-directory "/diary.org")
      org-default-notes-file
      (concat org-directory "/refile.org"))

(setq org-src-preserve-indentation t)

(setq
 gdb-many-windows t
 gdb-show-main t)

(setq deft-directory "~/Dropbox/Org")

;; (evil-define-key 'normal cider-mode-map (kbd "gd") #'cider-find-var)
;; (evil-define-key 'normal cider-mode-map (kbd "C-o") #'cider-pop-back)

;; (evil-define-key 'insert clojure-mode-map (kbd "C-p") nil)
;; (evil-define-key 'insert clojure-mode-map (kbd "C-n") nil)

;; (after! clojure-mode
;;   (evil-define-key 'insert cider-mode-map (kbd "C-p") nil)
;;   (evil-define-key 'insert cider-mode-map (kbd "C-n") nil)
;;   )

;; (add-hook 'cider-mode-hook
;;           (lambda()
;;           (evil-define-key 'insert cider-mode-map (kbd "C-p") nil)
;;           (evil-define-key 'insert cider-mode-map (kbd "C-n") nil)
;;             ))

;; (after! evil-org
;;   (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))


(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

(global-auto-revert-mode)

(setq c-syntactic-indentation nil)

(map!
 :map (org-mode-map)
 :i "<S-return>" #'org-insert-subheading)

;; (map!
;;  :map (cider-mode-map)
;;  :i "<C-p>" nil
;;  :i "<C-n>" nil
;;  )

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
    :n "e" #'expand-member-functions
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
(define-key evil-normal-state-map (kbd "g t")
  'centaur-tabs-forward)
(define-key evil-normal-state-map (kbd "g T")
  'centaur-tabs-backward)

(setq lsp-file-watch-threshold nil)

(tooltip-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(use-package! company-tabnine
  :after company
  :config
  (set-company-backend! '(prog-mode conf-mode org-mode elisp-mode) '(company-yasnippet :with company-capf :with company-tabnine))
  (setq company-backends '((company-yasnippet :with company-capf)))
  )

;; (setq +lsp-company-backends '(company-capf :with company-yasnippet :with company-tabnine))
(setq +lsp-company-backends '(company-yasnippet :with company-capf))

(define-key evil-insert-state-map (kbd "C-n") 'company-select-next-or-abort)
(define-key evil-insert-state-map (kbd "C-p") 'company-select-previous-or-abort)

(after!
  company
  (setq company-minimum-prefix-length
        2
        company-tooltip-limit
        25)
  (define-key! company-active-map
    "TAB" nil
    [tab] nil))

(add-hook! company-mode
  (setq company-transformers '(company-sort-by-backend-importance))
  )

(defmacro set-evil-number-keymap (key-set func &rest modes)
  `(progn
     ,@(-map
        (lambda (mode)
          `(define-key ,(intern (concat "evil-" mode "-state-map")) (kbd ,key-set)
             ',(intern
                (concat "evil-numbers/" func))))
        ,modes)))
;; (set-evil-number-keymap "C-a" "inc-at-pt" "normal" "insert")
;; (set-evil-number-keymap "C-x" "dec-at-pt" "normal" "insert")
(progn (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt) (define-key evil-insert-state-map (kbd "C-a") 'evil-numbers/inc-at-pt))
(progn (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt) (define-key evil-insert-state-map (kbd "C-x") 'evil-numbers/dec-at-pt))

(after! yasnippet
  (add-to-list 'yas-snippet-dirs "~/.snippets")
  )

(after! auto-yasnippet
  (setq aya-persist-snippets-dir "~/.snippets")
  )
(after! evil-mc
  (add-to-list 'evil-mc-incompatible-minor-modes 'lispy-mode))

;; (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq display-line-numbers-type 'relative)

(setq url-debug t)

(setq leetcode-prefer-language "cpp")
(setq leetcode-prefer-sql "mysql")

(setq org-html-htmlize-output-type 'css)

;; (setq js2-basic-offset 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)

(let ((liberime-auto-build t))
  (require 'liberime nil t))

(use-package! liberime)
(use-package! pyim
  ;; :quelpa (pyim :fetcher github :repo "merrickluo/pyim")
  :init
  (setq pyim-title "R")
  :config
  ;; (use-package pyim-basedict
  ;;   :config
  ;;   (pyim-basedict-enable))
  (define-key evil-insert-state-map (kbd "M-i") 'pyim-convert-string-at-point)
  (setq pyim-dcache-auto-update t)
  (setq default-input-method "pyim")

  (setq pyim-page-length 9)

  ;; 我使用全拼
  (setq pyim-page-tooltip 'child-frame)

  (setq pyim-default-scheme 'rime)
  (liberime-try-select-schema "luna_pinyin_simp")
  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
		'(pyim-probe-dynamic-english
		  pyim-probe-isearch-mode
		  pyim-probe-program-mode
                  pyim-probe-evil-normal-mode
		  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
		'(pyim-probe-punctuation-line-beginning
		  pyim-probe-punctuation-after-punctuation)))

(setq org-re-reveal-revealjs-version "4.0")

(require 'ox-confluence-en)

(require 'systemd)

(setq x-enable-primary t)

;; (use-package wakatime-mode :ensure t)
(global-wakatime-mode)


(add-hook! wakatime-mode
 (setq wakatime-cli-path "wakatime")
  )

;; (require 'org-tempo)
;; (after! 'org (add-to-list  'org-structure-template-alist
;;                      '("b" "#+BEGIN_SRC shell\n?\n#+END_SRC")
;;                            ))
(after! lsp-clients
  (set-lsp-priority! 'clangd 1))

(setq haskell-process-type 'cabal-new-repl)


(setq-default fill-column 120)


(setq +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn)

(setq lsp-ui-doc-use-webkit t)

(setq lsp-ui-doc-max-height 99)
(setq lsp-ui-doc-max-width 9999)


;; (add-hook 'emacs-startup-hook (lambda () (normal-erase-is-backspace-mode +1)))

(if (not (display-graphic-p)) (setq normal-erase-is-backspace t))


;;  python
;;
(setq lsp-pyls-plugins-autopep8-enabled nil)
(setq lsp-pyls-plugins-yapf-enabled t)
