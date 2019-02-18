;; -*- mode: emacs-lisp -*-
(defun dotspacemacs/layers ()
  (setq-default dotspacemacs-distribution 'spacemacs
                dotspacemacs-enable-lazy-installation
                `unused
                dotspacemacs-ask-for-lazy-installation
                t
                dotspacemacs-configuration-layer-path
                '()
                dotspacemacs-configuration-layers
                '((auto-completion :variables auto-completion-tab-key-behavior
                                   nil auto-completion-return-key-behavior 'complete
                                   auto-completion-complete-with-key-sequence-delay
                                   0.0 auto-completion-idle-delay 0.0 auto-completion-enable-snippets-in-popup
                                   t auto-completion-enable-help-tooltip t auto-completion-private-snippets-directory
                                   "~/.snippets") python
                                   (clojure :variables clojure-enable-fancify-symbols
                                            t clojure-enable-sayid t clojure-enable-clj-refactor
                                            t)
                                   chrome
                                   (chinese :variables chinese-enable-fcitx
                                            t chinese-enable-youdao-dict t)
                                   (c-c++ :variables c-c++-enable-clang-support
                                          t c-c++-default-mode-for-headers 'c++-mode)
                                   docker
                                   dash
                                   epub
                                   (elfeed :variables rmh-elfeed-org-files
                                           (list "~/Dropbox/Org/elfeed.org"))
                                   (emacs-lisp :variables emacs-lisp-hide-namespace-prefix
                                               t)
                                   (git :variables
                                        git-magit-status-fullscreen t
                                        git-enable-github-support t
                                        git-gutter-use-fringe t)
                                   github
                                   (gtags :variables gtags-enable-by-default
                                          t)
                                   (haskell :variables haskell-completion-backend'intero)
                                   (html :variables web-fmt-tool'prettier)
                                   (javascript :variables javascript-disable-tern-port-files
                                               t)
                                   (markdown :variables markdown-live-preview-engine'vmd)
                                   (mu4e :variables mu4e-use-maildirs-extension
                                         nil mu4e-enable-async-operations t mu4e-enable-mode-line
                                         t mu4e-enable-notifications t)
                                   nixos
                                   osx
                                   (org :variables org-want-todo-bindings
                                        t org-enable-reveal-js-support t org-enable-org-journal-support
                                        t org-projectile-file "~/Dropbox/Org/Projects.org")
                                   pdf
                                   (purescript :variables purescript-enable-rebuild-on-save
                                               t)
                                   ruby-on-rails
                                   react
                                   restclient
                                   ;; (multiple-cursors :variables multiple-cursors-backend 'evil-mc)
                                   semantic
                                   (shell :variables shell-default-shell'eshell
                                          shell-enable-smart-eshell t)
                                   (spell-checking :variables spell-checking-enable-auto-dictionary
                                                   nil enable-flyspell-auto-completion t)
                                   (syntax-checking :variables syntax-checking-enable-by-default
                                                    nil syntax-checking-enable-tooltips t)
                                   (templates :variables templates-private-directory
                                              "~/.templates")
                                   (typescript :variables typescript-fmt-on-save
                                               t)
                                   ;; rust
                                   ;; plantuml
                                   (scala :variables scala-use-unicode-arrows
                                          t)
                                   ;; go
                                   (version-control :variables
                                                    version-control-diff-tool 'diff-hl
                                                    version-control-global-margin t)
                                   vimscript
                                   yaml)
                dotspacemacs-additional-packages
                '(realgud realgud-pry exec-path-from-shell)
                dotspacemacs-frozen-packages
                '()
                dotspacemacs-excluded-packages
                '(evil-escape)
                dotspacemacs-install-packages
                'used-only))
(defun dotspacemacs/init ()
  (setq-default dotspacemacs-check-for-update nil
                dotspacemacs-elpa-subdirectory
                nil
                dotspacemacs-editing-style
                '(hybrid :variables hybrid-mode-enable-evilified-state
                         t hybrid-mode-enable-hjkl-bindings nil hybrid-mode-use-evil-search-module
                         nil hybrid-mode-default-state 'normal)
                dotspacemacs-verbose-loading
                nil
                dotspacemacs-startup-banner
                nil
                dotspacemacs-startup-lists
                '((recents . 5)
                  (projects . 7))
                dotspacemacs-startup-buffer-responsive
                t
                dotspacemacs-scratch-mode
                'text-mode
                dotspacemacs-themes
                '(spacemacs-dark molokai)
                dotspacemacs-colorize-cursor-according-to-state
                t
                dotspacemacs-default-font
                '("Source Code Pro" :size 14
                  :weight normal
                  :width normal
                  :powerline-scale 1.1)
                dotspacemacs-mode-line-theme
                '(spacemacs :separator wave
                            :separator-scale 2)
                dotspacemacs-leader-key
                "SPC"
                dotspacemacs-emacs-command-key
                "SPC"
                dotspacemacs-ex-command-key
                ":"
                dotspacemacs-emacs-leader-key
                "M-m"
                dotspacemacs-major-mode-leader-key
                ","
                dotspacemacs-major-mode-emacs-leader-key
                "C-M-m"
                dotspacemacs-distinguish-gui-tab
                t
                dotspacemacs-remap-Y-to-y$
                nil
                dotspacemacs-retain-visual-state-on-shift
                t
                dotspacemacs-enable-emacs-pdumper
                nil
                dotspacemacs-visual-line-move-text
                nil
                dotspacemacs-ex-substitute-global
                nil
                dotspacemacs-default-layout-name
                "Bitmain"
                dotspacemacs-display-default-layout
                t
                dotspacemacs-auto-resume-layouts
                t
                dotspacemacs-large-file-size
                0.5
                dotspacemacs-auto-save-file-location
                'original
                dotspacemacs-max-rollback-slots
                5
                dotspacemacs-helm-resize
                nil
                dotspacemacs-helm-no-header
                t
                dotspacemacs-helm-position
                'bottom
                dotspacemacs-helm-use-fuzzy
                'always
                dotspacemacs-enable-paste-transient-state
                nil
                dotspacemacs-which-key-delay
                0.4
                dotspacemacs-which-key-position
                'bottom
                dotspacemacs-loading-progress-bar
                t
                dotspacemacs-fullscreen-at-startup
                nil
                dotspacemacs-fullscreen-use-non-native
                t
                dotspacemacs-maximized-at-startup
                t
                dotspacemacs-active-transparency
                90
                dotspacemacs-inactive-transparency
                90
                dotspacemacs-show-transient-state-title
                t
                dotspacemacs-show-transient-state-color-guide
                t
                dotspacemacs-mode-line-unicode-symbols
                t
                dotspacemacs-mode-line-theme
                'spacemacs
                dotspacemacs-smooth-scrolling
                t
                dotspacemacs-line-numbers
                '(:relative t :disabled-for-modes dired-mode
                            doc-view-mode markdown-mode pdf-view-mode
                            text-mode :size-limit-kb 100)
                dotspacemacs-folding-method
                'evil
                dotspacemacs-smartparens-strict-mode
                t
                dotspacemacs-smart-closing-parenthesis
                t
                dotspacemacs-highlight-delimiters
                'all
                dotspacemacs-enable-server
                t
                dotspacemacs-persistent-server
                t
                dotspacemacs-search-tools
                '("pt" "grep")
                dotspacemacs-default-package-repository
                nil
                dotspacemacs-whitespace-cleanup
                'trailing
                dotspacemacs-pretty-docs
                t))
(defun dotspacemacs/user-init ()
  ;; (if (eq system-type 'gnu/linux)
  ;;     (setq-default dotspacemacs-default-font '("Source Code Pro" :size 25
  ;;                                               :weight normal
  ;;                                               :width normal
  ;;                                               :powerline-scale 2.2)))
  )
(defun dotspacemacs/user-config ()
  (setq auto-insert-query nil)
  (exec-path-from-shell-initialize)
  (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hooks)
  ;; Configure flycheck to use Nix
  ;; https://github.com/travisbhartwell/nix-emacs#flycheck
  ;; Requires `nix-sandbox` package added to dotspacemacs-additional-packages
  ;; (setq flycheck-command-wrapper-function
  ;;       (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command)))
  ;; (setq flycheck-executable-find
  ;;       (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))

  ;; (global-set-key [(control ?h)] 'delete-backward-char)
  ;; ;; Configure haskell-mode (haskell-cabal) to use Nix
  ;; (setq haskell-process-wrapper-function
  ;;       (lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))

  ;; ;; Configure haskell-mode to use cabal new-style builds
  ;; (setq haskell-process-type 'cabal-new-repl)

  ;; ;; We have limit flycheck to haskell because the above wrapper configuration is global (!)
  ;; ;; FIXME: How? Using mode local variables?
  ;; (setq flycheck-global-modes '(haskell-mode))
  (setq haskell-hoogle-url "http://localhost:8080/?hoogle=%s")
  (setq haskell-hoogle-command nil)
  (auto-save-visited-mode 1)
  (setq auto-save-visited-interval 1)
  (defmacro indentation-fix (m-name indent-function)
    `(with-eval-after-load ,(seq-concatenate 'string m-name "-mode")
       (defun ,(intern (seq-concatenate 'string m-name "-evil-open-above")) ()
         (interactive)
         (evil-digit-argument-or-evil-beginning-of-line)
         ,(list indent-function)
         (evil-previous-line)
         (,(intern (seq-concatenate 'string m-name "-indentation-indent-line")))
         (evil-append-line nil))
       (defun ,(intern (seq-concatenate 'string m-name "-evil-open-below")) ()
         (interactive)
         (evil-append-line nil)
         ,(list indent-function))
       (evil-define-key 'normal
         haskell-mode-map
         "o"
         ',(intern (seq-concatenate 'string m-name "-evil-open-below"))
         "O"
         ',(intern (seq-concatenate 'string m-name "-evil-open-above")))))
  (indentation-fix "haskell" haskell-indentation-newline-and-indent)
  (indentation-fix "purescript" purescript-newline-and-indent)
  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let ((eslint-path (if (projectile-project-p)
                           (concat (projectile-project-root)
                                   "node_modules/eslint/bin/eslint.js"))))
      (if (file-exists-p eslint-path)
          (progn
            (message eslint-path)
            (setq flycheck-javascript-eslint-executable
                  eslint-path)))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (setq js2-include-node-externs t)
  (setq spacemacs-buffer--warnings nil)
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  (setq require-final-newline nil)
                                        ; Persistent UNDO Tree
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(concat spacemacs-cache-directory "undo"))))
  (unless (file-exists-p (concat spacemacs-cache-directory "undo"))
    (make-directory (concat spacemacs-cache-directory "undo")))
  ;;Set key jump only one line
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (normal-erase-is-backspace-mode 1)
  (global-set-key "\C-h" 'delete-backward-char)
  (global-set-key (kbd "M-m x x")
                  'youdao-dictionary-search-at-point+)
  (setq magit-repository-directories '("~/personal" "~/workplace" "~/test"))
  ;;autocomplete
  (global-company-mode t)
  (setq ensime-startup-notification nil)
  (setq ensime-startup-snapshot-notification
        nil)
  ;;mu4e
  ;; give me ISO(ish) format date-time stamps in the header list
  (setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
  (setq mu4e-attachment-dir "~/Downloads/" mu4e-maildir
        "~/maildir/" mu4e-get-mail-command "mbsync -a -q"
        mu4e-update-interval 100 mu4e-view-show-images
        t mu4e-view-prefer-html t mu4e-sent-messages-behavior
        'delete message-kill-buffer-on-exit t mu4e-headers-auto-update
        t org-mu4e-link-query-in-headers-mode nil
        mu4e-html2text-command "w3m -dump -T text/html")
  ;; (setq send-mail-function 'message-send-mail-with-sendmail)
  (setq vc-follow-symlinks nil)
  ;; (setq sendmail-program "msmtp")
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls
        t
        user-mail-address
        "xiongchenyu6@gmail.com"
        smtpmail-starttls-credentials
        '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials
        (expand-file-name "~/.authinfo.gpg")
        smtpmail-default-smtp-server
        "smtp.gmail.com"
        smtpmail-smtp-server
        "smtp.gmail.com"
        smtpmail-smtp-service
        587
        smtpmail-debug-info
        t)
  (with-eval-after-load 'mu4e-alert
    (mu4e-alert-set-default-style 'notifier))
  ;; tell msmtp to choose the SMTP server according to the from field in the outgoing email
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-sendmail-f-is-evil 't)
  ;; convert org mode to HTML automatically
  (setq org-mu4e-convert-to-html t)
  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
  (require 'realgud)
  (require 'gnus-dired)
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer (dolist (buffer (buffer-list t))
                             (set-buffer buffer)
                             (when (and (derived-mode-p 'message-mode)
                                        (null message-sent-message-via))
                               (push (buffer-name buffer)
                                     buffers))))
      (nreverse buffers)))
  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
  (menu-bar-mode 1)
  (setq indent-guide-global-mode t)
  ;; gpg
  (setq epg-gpg-program "gpg2")
  (add-hook 'mu4e-compose-mode-hook 'org-mu4e-compose-org-mode)
  (add-hook 'mu4e-view-mode-hook 'epa-mail-mode)
  ;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  ;;parabox
  (setq paradox-github-token '3db959a368a082f4290d0c81313e46418d29f199)
  (setq org-journal-dir "~/Dropbox/Org/journal/")
  (setq org-directory "~/Dropbox/Org"
        org-agenda-files
        (list org-directory)
        org-agenda-diary-file
        (concat org-directory "/diary.org")
        org-default-notes-file
        (concat org-directory "/refile.org"))
  ;; Set to the name of the file where new notes will be stored
  (setq org-mobile-inbox-for-pull (concat org-directory "/refile.org"))
  ;; Set to <your Dropbox root directory>/MobileOrg.
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  (setq org-latex-compiler "latexmk -pdf %f")
  (setq org-ref-default-bibliography '("~/Dropbox/Org/Papers/references.bib")
        org-ref-pdf-directory
        "~/Dropbox/Org/Papers/"
        org-ref-bibliography-notes
        "~/Dropbox/Org/Papers/notes.org")
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("koma-article" "\\documentclass{scrartcl}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  (setq org-babel-python-command "python3")
  (setq python-shell-interpreter "python3")
  (setq org-plantuml-jar-path "~/Dropbox/plantuml.jar")
  (with-eval-after-load 'org
    (setq org-confirm-babel-evaluate nil)
    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((js . t)
                                   (emacs-lisp . t)
                                   (C . t)
                                   (python . t)
                                   (plantuml . t)
                                   (gnuplot . t)
                                   (haskell . t)
                                   (shell . t)
                                   (dot . t)))
    (add-to-list 'org-src-lang-modes
                 (quote ("plantuml" . fundamental))))
  (setq create-lockfiles nil)
  (setq edit-server-url-major-mode-alist '(("github\\.com" . org-mode)))
  (add-to-list 'load-path "~")
  (setq
   ;; use gdb-many-windows by default
   gdb-many-windows t

   ;; Non-nil means display source file containing the main routine at startup
   gdb-show-main t
   )
  ;; (require 'tidal)
  )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(realgud-pry realgud test-simple loc-changes load-relative robe helm zeal-at-point youdao-dictionary yatemplate yasnippet-snippets yapfify yaml-mode xterm-color ws-butler writeroom-mode winum which-key web-mode web-beautify volatile-highlights vmd-mode vimrc-mode vi-tilde-fringe uuidgen use-package treemacs-projectile treemacs-evil toc-org tide tagedit symon string-inflection stickyfunc-enhance srefactor spaceline-all-the-icons smeargle slim-mode shell-pop seeing-is-believing scss-mode sayid sass-mode rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop rspec-mode rjsx-mode reveal-in-osx-finder restclient-helm restart-emacs rbenv rainbow-delimiters pyvenv pytest pyim pyenv-mode py-isort pug-mode psci psc-ide projectile-rails prettier-js pippel pipenv pip-requirements persp-mode pdf-tools password-generator parinfer paradox pangu-spacing ox-reveal overseer osx-trash osx-dictionary orgit org-projectile org-present org-pomodoro org-mime org-journal org-download org-bullets org-brain open-junk-file ob-restclient ob-http nov nix-sandbox nix-mode nameless mvn multi-term mu4e-maildirs-extension mu4e-alert move-text molokai-theme mmm-mode minitest meghanada maven-test-mode markdown-toc magithub magit-svn magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode link-hint launchctl json-navigator js2-refactor js-doc intero indent-guide importmagic impatient-mode hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-rtags helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-nixos-options helm-mu helm-mode-manager helm-make helm-hoogle helm-gtags helm-gitignore helm-git-grep helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets groovy-mode groovy-imports gradle-mode google-translate google-c-style golden-ratio gnuplot gmail-message-mode gitignore-templates github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md ggtags fuzzy forge font-lock+ flyspell-popup flyspell-correct-helm flymd flycheck-rtags flycheck-pos-tip flycheck-haskell flx-ido find-by-pinyin-dired fill-column-indicator feature-mode fcitx fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-ediff evil-cleverparens evil-args evil-anzu eshell-z eshell-prompt-extras esh-help ensime emmet-mode elisp-slime-nav elfeed-web elfeed-org elfeed-goodies editorconfig edit-server dumb-jump dotenv-mode doom-modeline dockerfile-mode docker disaster diminish diff-hl dactyl-mode cython-mode counsel-projectile company-web company-tern company-statistics company-rtags company-restclient company-quickhelp company-nixos-options company-emacs-eclim company-cabal company-c-headers company-anaconda column-enforce-mode cmm-mode clojure-snippets clj-refactor clean-aindent-mode clang-format cider-eval-sexp-fu chruby chinese-conv centered-cursor-mode bundler browse-at-remote auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent ace-pinyin ace-link ace-jump-helm-line ac-ispell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
