;; -*- mode: emacs-lisp -*-
(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation `unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(csv
     sql
     python
     git
     github
     version-control
     imenu-list ;; space b i
     (markdown :variables markdown-live-preview-engine 'vmd)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'multiterm)
     dash
     osx
     (gtags :variables gtags-enable-by-default t)
     (erc :variables
          erc-server-list
          '(("irc.freenode.net"
             :port "6697"
             :ssl t
             :nick "FreemanXiong")))
      (colors :variables
             colors-enable-nyan-cat-progress-bar t)
     fasd
     (chinese :variables
               chinese-enable-youdao-dict nil)
     spell-checking
     (mu4e :variables
           mu4e-enable-mode-line t
           mu4e-alert-set-default-style 'notifier
           mu4e-enable-notifications t)
     (auto-completion :variables
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-return-key-behavior nil
                      auto-completion-complete-with-key-sequence-delay 0.0
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t)
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil)
     (wakatime :variables wakatime-api-key "06fb08d0-68a4-4b39-bbb0-d34d325dc046"
               ;; use the actual wakatime path
               wakatime-cli-path "/usr/local/bin/wakatime")
     (org :variables
          org-enable-bootstrap-support t
          org-enable-github-support t
          org-enable-reveal-js-support t
          org-projectile-file "~/Dropbox/Org/Projects.org")
     yaml
     typescript
     (javascript :variables javascript-disable-tern-port-files nil)
     purescript
     scala
     html
     plantuml
     graphviz
     emacs-lisp
     latex
     (haskell :variables
              haskell-process-type 'stack-ghci
              haskell-completion-backend 'intero)
     (geolocation :variables
                  geolocation-enable-automatic-theme-changer nil
                  geolocation-enable-location-service nil
                  geolocation-enable-weather-forecast t)
     )
   dotspacemacs-additional-packages
   '(
     shakespeare-mode
     )
   dotspacemacs-frozen-packages
   '()
   dotspacemacs-excluded-packages
   '(evil-escape
     )
   dotspacemacs-install-packages
   'used-only))
(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'hybrid
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '((todos . 5) (agenda . 5) (recents . 3)(projects . 3))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(spacemacs-dark
                         molokai)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '(
                               "Source Code Pro" :size 13
                               ;; "InconsolataForPowerline Nerd Font" :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 0.5
   dotspacemacs-auto-save-file-location 'original
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native t
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers '(:relative t :disabled-for-modes dired-mode
                                         doc-view-mode markdown-mode org-mode pdf-view-mode
                                         text-mode :size-limit-kb 1000)
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("pt" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing))
(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first.")
(defun dotspacemacs/user-config ()
  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let ((eslint-path (if (projectile-project-p)
                           (concat (projectile-project-root) "node_modules/eslint/bin/eslint.js"))))
      (if (file-exists-p eslint-path)
          (progn
            (message eslint-path)
            (setq flycheck-javascript-eslint-executable eslint-path)))))

  (global-set-key [(control ?h)] 'delete-backward-char)
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (setq js2-include-node-externs t)
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is

you should place your code here."
  (setq spacemacs-buffer--warnings nil)
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  (setq require-final-newline nil)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d.undo/undo")))
  ;;Set key jump only one line
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  (setq exec-path (append exec-path '("/usr/local/bin")))
  (setq exec-path (append exec-path '("/usr/local/sbin")))
  (add-hook 'prog-mode-hook 'spacemacs/toggle-hungry-delete-on)
  (setq ensime-startup-notification nil)
  (setq ensime-startup-snapshot-notification nil)
  (add-hook 'java-mode-hook 'ensime-mode)

  (setq magit-repository-directories '("~/Github/"))

  ;;autocomplete
  (global-company-mode t)

  ;;ranger settings
  (setq ranger-cleanup-on-disable t)
  (setq ranger-cleanup-eagerly t)
  (setq ranger-show-hidden nil)
  (setq ranger-show-literal nil)
  (setq ranfasdger-ignored-extensions '("mkv" "iso" "mp4" "avi"))
  (setq ranger-max-preview-size 10)
  (setq haskell-font-lock-symbols t)
  ;; haskell
  (defun haskell-evil-open-above ()
    (interactive)
    (evil-digit-argument-or-evil-beginning-of-line)
    (haskell-indentation-newline-and-indent)
    (evil-previous-line)
    (haskell-indentation-indent-line)
    (evil-append-line nil))

  (defun haskell-evil-open-below ()
    (interactive)
    (evil-append-line nil)
    (haskell-indentation-newline-and-indent))

  (evil-define-key 'normal haskell-mode-map "o" 'haskell-evil-open-below
    "O" 'haskell-evil-open-above)

  ;; ERC CONFIG
  (spacemacs|use-package-add-hook erc
    :post-config
    (progn
      ;; if imagemagick isn't supported, we don't want inline images
      (unless (fboundp 'imagemagick-types)
        (setq erc-modules (-remove-item 'image erc-modules)))

      (setq erc-autojoin-channels-alist
            '(("freenode.net" "#haskell-beginners"))
            erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE" "353")
            erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE" "353")
            erc-track-exclude-server-buffer t
            erc-track-position-in-mode-line t
            erc-join-buffer 'bury
            erc-hl-nicks-minimum-contrast-ratio 2.5
            erc-hl-nicks-color-contrast-strategy '(invert contrast)
            erc-fill-column 120
            erc-fill-function 'erc-fill-static
            erc-fill-static-center 20
            erc-current-nick-highlight-type 'all
            erc-log-insert-log-on-open nil
            erc-track-shorten-aggressively 'max)

      (add-hook 'erc-mode-hook 'turn-off-show-smartparens-mode)))

  ;;For better search use C-w
  (defun helm-yank-text-at-point--move-to-beginning (orig-func &rest args)
    "Initialize `helm-yank-point' to the beginning of word at point."
    (unless helm-yank-point
      (setq helm-yank-point
            (with-helm-current-buffer
              (save-excursion
                (let ((fwd-fn (or helm-yank-text-at-point-function #'forward-word)))
                  (funcall fwd-fn -1))
                (point)))))
    (apply orig-func args))

  (advice-add 'helm-yank-text-at-point :around
              #'helm-yank-text-at-point--move-to-beginning)
  (setq erc-image-inline-rescale 400)
  ;;mu4e
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-attachment-dir "~/Downloads/"
        mu4e-maildir "~/Mail"
        mu4e-get-mail-command "mbsync -a -q"
        mu4e-update-interval 100
        mu4e-view-show-images  t
        mu4e-view-prefer-html t
        mu4e-sent-messages-behavior 'delete
        message-kill-buffer-on-exit t
        mu4e-headers-auto-update t
        org-mu4e-link-query-in-headers-mode nil
        ;;mu4e-html2text-command "w3m -dump -T text/html"
        )
  ;;use msmtp
  (setq send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp")
  (setq mu4e-alert-set-default-style 'notifier)
  ;; tell msmtp to choose the SMTP server according to the from field in the outgoing email
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-sendmail-f-is-evil 't)
  ;; convert org mode to HTML automatically
  (setq org-mu4e-convert-to-html t)
  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
  (require 'gnus-dired)
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))
  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

  ;; gpg
  (setq epg-gpg-program "gpg2")
  (add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
  (add-hook 'mu4e-view-mode-hook 'epa-mail-mode)

  ;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)

  ;;dash settings
  (setq helm-dash-browser-func 'eww)
  ;;geolocation settings
  (setq sunshine-appid "da749c5e70ad565dea92c3de52683711")
  (setq sunshine-location "050335,SG")
  (setq sunshine-show-icons t)
  (setq sunshine-units 'metric)
  ;;parabox
  (setq paradox-github-token '3db959a368a082f4290d0c81313e46418d29f199)
  ;;ledger settins
  (setq inhibit-read-only t)
  (setq org-directory "~/Dropbox/Org"
        org-agenda-files (list org-directory)
        org-agenda-diary-file (concat org-directory "/diary.org")
        org-default-notes-file (concat org-directory "/refile.org"))
  ;; Set to the name of the file where new notes will be stored
  (setq org-mobile-inbox-for-pull (concat org-directory "/refile.org"))
  ;; Set to <your Dropbox root directory>/MobileOrg.
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  (setq org-latex-compiler "pdflatex")
  (require 'ox-latex)
  (setq org-latex-caption-above '(table image))
  (add-to-list 'org-latex-classes
               '("koma-article"
                 "\\documentclass{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-publish-project-alist
        '(("orgfiles"
           :base-directory "~/Dropbox/Org/"
           :base-extension "org"
           :publishing-directory "~/html/"
           :publishing-function org-twbs-publish-to-html
           :with-sub-superscript nil
           :recursive t
           :timestamp t
           :exclude-tags "noexport"
           :auto-sitemap t                ; Generate sitemap.org automagically...
           :sitemap-filename "index.org"  ; ... call it sitemap.org (it's the default)...
           :sitemap-title "Freeman's notebook"
           :sitemap "index.html"
           :author "Freeman"
           :email "xiongchenyu6@gamil.com"
           :time-stamp-file t
           :makeindex t
           :with-toc t
           :with-timestamps t
           :html-link-home "index.html")
          ("blog-static"
           :base-directory "~/Dropbox/Org"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/html/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("website" :components ("orgfiles" "blog-static"))))
  (setq org-export-date-timestamp-format "%Y-%m-%d")
  (setq org-twbs-postamble 't)
  (setq org-twbs-postamble-format
        '(("en" "<div id='disqus_thread'></div>
            <script type='text/javascript'>
                var disqus_shortname = 'blogfreeman';
                (function() {
                    var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
                    dsq.src = 'https://' + disqus_shortname + '.disqus.com/embed.js';
                    (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
                })();
            </script>
            <noscript>Please enable JavaScript to view the <a href='https://disqus.com/?ref_noscript' rel='nofollow'>comments powered by Disqus.</a></noscript>
            <p class='author'>Author: %a (%e)</p><p>Exported At %T. Created by %c </p>
            <a href='#' class='back-to-top' id='fixed-back-to-top' style='display: inline;'></a>"
           )))
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-deadline-warning-days 14)
  (setq org-agenda-start-on-weekday nil)
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red"
           :weight bold)
          ("NEXT" :foreground "blue"
           :weight bold)
          ("DONE" :foreground "forest green"
           :weight bold)
          ("WAITING" :foreground "orange"
           :weight bold)
          ("HOLD" :foreground "magenta"
           :weight bold)
          ("CANCELLED" :foreground "forest green"
           :weight bold)))
  (setq org-todo-state-tags-triggers
        '(("CANCELLED"
           ("CANCELLED" . t))
          ("WAITING"
           ("WAITING" . t))
          ("HOLD"
           ("WAITING")
           ("HOLD" . t))
          (done ("WAITING")
                ("HOLD"))
          ("TODO"
           ("WAITING")
           ("CANCELLED")
           ("HOLD"))
          ("NEXT"
           ("WAITING")
           ("CANCELLED")
           ("HOLD"))
          ("DONE"
           ("WAITING")
           ("CANCELLED")
           ("HOLD"))))
  ;; Tags with fast selection keys
  (setq org-tag-alist
        '((:startgroup)
          ("@errand" . ?e)
          ("@office" . ?o)
          ("@home" . ?H)
          ("@school" . ?s)
          (:endgroup)
          ("WAITING" . ?w)
          ("HOLD" . ?h)
          ("PERSONAL" . ?P)
          ("WORK" . ?W)
          ("FARM" . ?F)
          ("ORG" . ?O)
          ("crypt" . ?E)
          ("NOTE" . ?n)
          ("CANCELLED" . ?c)
          ("FLAGGED" . ??)))

  ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
  (setq org-capture-templates
        '(("t" "todo"
           entry
           (file "~/Dropbox/Org/refile.org")
           "* TODO %?\n%U\n%a\n"
           :empty-lines 1
           :clock-in t
           :clock-resume t)
          ("l" "link-note"
           entry
           (file "~/Dropbox/Org/refile.org")
           "* %? :NOTE:\n%U\n%a\n"
           :empty-lines 1)
          ("n" "note"
           entry
           (file "~/Dropbox/Org/refile.org")
           "* %? :NOTE:\n%U\n%c\n"
           :prepend t
           :kill-buffer t
           :empty-lines 1)
          ("s" "Code Snippet"
           entry
           (file org-agenda-file-code-snippet)
           "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
          ("h" "Habit"
           entry
           (file "~/Dropbox/Org/refile.org")
           "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"
           :empty-lines 1)))
  ;; Remove empty LOGBOOK drawers on clock out
  (defun bh/remove-empty-drawer-on-clock-out ()
    (interactive)
    (save-excursion
      (beginning-of-line 0)
      (org-remove-empty-drawer-at "LOGBOOK" (point))))
  (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

  ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  ;; Do not dim blocked tasks
  (setq org-agenda-dim-blocked-tasks nil)
  ;; Compact the block agenda view
  (setq org-agenda-compact-blocks t)
  ;; Custom agenda command definitions
  (setq org-agenda-custom-commands
        '(("N" "Notes"
           tags
           "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("h" "Habits"
           tags-todo
           "STYLE=\"habit\""
           ((org-agenda-overriding-header "Habits")
            (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
          ))
  (setq org-fast-tag-selection-single-key (quote expert))
  (setq spaceline-org-clock-p t)
  (setq org-babel-python-command "python3")
  (setq python-shell-interpreter "python3")
  (org-babel-do-load-languages
   (quote org-babel-load-languages)
   (quote ((emacs-lisp . t)
           (ditaa . t)
           (python . t)
           (gnuplot . t)
           (haskell . t)
           (shell . t)
           (scala . t)
           (c . t)
           (dot . t)
           (plantuml . t)
           (js . t))))
  (setq org-agenda-persistent-filter t)
  (setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.10/libexec/ditaa0_10.jar")
  (setq org-plantuml-jar-path "~/plantuml.jar")
  (setq org-confirm-babel-evaluate nil)
  (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental))))
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
   (quote
    (auctex-latexmk yapfify yaml-mode xterm-color ws-butler winum which-key web-mode web-beautify wakatime-mode volatile-highlights vmd-mode vi-tilde-fringe uuidgen use-package toc-org tide tagedit symon sunshine string-inflection sql-indent spaceline smeargle slim-mode shell-pop shakespeare-mode scss-mode sass-mode reveal-in-osx-finder restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters pyvenv pytest pyim pyenv-mode py-isort pug-mode psci psc-ide popwin plantuml-mode pip-requirements persp-mode pcre2el pbcopy password-generator paradox pangu-spacing ox-twbs ox-reveal ox-gfm overseer osx-trash osx-dictionary orgit org-projectile org-present org-pomodoro org-download org-bullets org-brain open-junk-file noflet neotree nameless mvn multi-term mu4e-maildirs-extension mu4e-alert move-text molokai-theme mmm-mode meghanada maven-test-mode markdown-toc magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint launchctl json-mode js2-refactor js-doc intero info+ indent-guide impatient-mode hy-mode hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-mode-manager helm-make helm-hoogle helm-gtags helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets groovy-mode groovy-imports graphviz-dot-mode gradle-mode google-translate golden-ratio gnuplot github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md ggtags fuzzy flyspell-correct-helm flycheck-pos-tip flycheck-haskell flx-ido find-by-pinyin-dired fill-column-indicator fasd fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-ediff evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks ensime emmet-mode elisp-slime-nav editorconfig dumb-jump diminish diff-hl dash-at-point dante cython-mode csv-mode company-web company-tern company-statistics company-quickhelp company-ghci company-ghc company-emacs-eclim company-cabal company-auctex company-anaconda column-enforce-mode color-identifiers-mode coffee-mode cmm-mode clean-aindent-mode browse-at-remote auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adaptive-wrap ace-window ace-pinyin ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
