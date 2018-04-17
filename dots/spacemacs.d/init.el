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
     git
     github
     version-control
     imenu-list ;; space b i
     (treemacs :variables
               treemacs-use-filewatch-mode t)
     (markdown :variables markdown-live-preview-engine 'vmd)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'ansi-term)
     (slack :variables
            slack-prefer-current-team t
            slack-display-team-name nil)
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
     emoji
     (chinese :variables
              chinese-enable-youdao-dict nil)
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     enable-flyspell-auto-completion t)
     (mu4e :variables
           mu4e-use-maildirs-extension nil
           mu4e-enable-async-operations t
           mu4e-enable-mode-line t
           mu4e-enable-notifications t)
     (auto-completion :variables
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-return-key-behavior nil
                      auto-completion-complete-with-key-sequence-delay 0.0
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t)
     syntax-checking
     (wakatime :variables wakatime-api-key "06fb08d0-68a4-4b39-bbb0-d34d325dc046"
               ;; use the actual wakatime path
               wakatime-cli-path "/usr/local/bin/wakatime")
     bibtex
     (org :variables
          org-enable-bootstrap-support t
          org-enable-github-support t
          org-enable-reveal-js-support t
          org-enable-org-journal-support t
          org-enable-hugo-support t
          org-projectile-file "~/Dropbox/Org/Projects.org")
     yaml
     typescript
     (javascript :variables javascript-disable-tern-port-files nil)
     html
     plantuml
     (scala :variables
            scala-use-unicode-arrows t)
     (java :variables java-backend 'ensime)
     idris
     php
     emacs-lisp
     clojure
     latex
     (haskell :variables
              haskell-process-type 'stack-ghci
              ;; haskell-completion-backend 'dante
              haskell-completion-backend 'intero
              ;; haskell-completion-backend 'ghci
              )
     (geolocation :variables
                  geolocation-enable-automatic-theme-changer nil
                  geolocation-enable-location-service nil
                  geolocation-enable-weather-forecast t))
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
   dotspacemacs-check-for-update t
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style '(hybrid :variables
                                       hybrid-mode-enable-evilified-state t
                                       hybrid-mode-enable-hjkl-bindings nil
                                       hybrid-mode-use-evil-search-module nil
                                       hybrid-mode-default-state 'normal)
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '((recents . 3)(projects . 3))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(spacemacs-dark
                         molokai)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '(
                               ;; "Source Code Pro" :size 12
                               "InconsolataForPowerline Nerd Font" :size 14
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
   dotspacemacs-display-default-layout t
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
   dotspacemacs-mode-line-theme 'spacemacs
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
  (setq exec-path-from-shell-arguments '("-l"))
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first.")
(defun dotspacemacs/user-config ()
  (setq mac-pass-command-to-system nil)

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
  (setq typescript-indent-level 2)
  (setq-default
   js2-basic-offset 2
   js-indent-level 2
   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2
   )

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

  (setq magit-repository-directories '("~/git/"))

  ;;autocomplete
  (global-company-mode t)

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

  (setq slack-enable-emoji t)
  (with-eval-after-load 'slack
    (when (file-exists-p "~/Dropbox/Org/emacs-slack.el")
      (load "~/Dropbox/Org/emacs-slack.el")))

  (setq ensime-startup-notification nil)
  (setq ensime-startup-snapshot-notification nil)
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

  ;; give me ISO(ish) format date-time stamps in the header list
  (setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
  (setq mu4e-attachment-dir "~/Downloads/"
        mu4e-maildir "~/Maildir/"
        mu4e-get-mail-command "mbsync -a -q"
        mu4e-update-interval 100
        mu4e-view-show-images  t
        mu4e-view-prefer-html t
        mu4e-sent-messages-behavior 'delete
        message-kill-buffer-on-exit t
        mu4e-headers-auto-update t
        org-mu4e-link-query-in-headers-mode nil
        mu4e-html2text-command "w3m -dump -T text/html"
        )
  ;; (setq send-mail-function 'message-send-mail-with-sendmail)
  (setq vc-follow-symlinks nil)
  ;; (setq sendmail-program "msmtp")
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        user-mail-address "xiongchenyu6@gmail.com"
        smtpmail-starttls-credentials
        '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials
        (expand-file-name "~/.authinfo.gpg")
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-debug-info t)

  (with-eval-after-load 'mu4e-alert
    (mu4e-alert-set-default-style 'notifier))
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
  (add-hook 'mu4e-compose-mode-hook 'org-mu4e-compose-org-mode)
  (add-hook 'mu4e-view-mode-hook 'epa-mail-mode)

  ;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)

  ;;geolocation settings
  (setq sunshine-appid "da749c5e70ad565dea92c3de52683711")
  (setq sunshine-location "050335,SG")
  (setq sunshine-show-icons t)
  (setq sunshine-units 'metric)
  ;;parabox
  (setq paradox-github-token '3db959a368a082f4290d0c81313e46418d29f199)
  ;;ledger settins
  (setq inhibit-read-only t)
  (setq org-journal-dir "~/Dropbox/Org/journal/")
  (setq org-directory "~/Dropbox/Org"
        org-agenda-files (list org-directory)
        org-agenda-diary-file (concat org-directory "/diary.org")
        org-default-notes-file (concat org-directory "/refile.org"))
  ;; Set to the name of the file where new notes will be stored
  (setq org-mobile-inbox-for-pull (concat org-directory "/refile.org"))
  ;; Set to <your Dropbox root directory>/MobileOrg.
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  (setq org-latex-compiler "latexmk -pdf %f")
  (setq org-ref-default-bibliography '("~/Dropbox/Org/Papers/references.bib")
        org-ref-pdf-directory "~/Dropbox/Org/Papers/"
        org-ref-bibliography-notes "~/Dropbox/Org/Papers/notes.org")
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("koma-article"
                   "\\documentclass{scrartcl}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  (setq org-publish-project-alist
        '(("orgfiles"
           :base-directory "~/Dropbox/Org/"
           :exclude "~/Dropbox/Org/journal"
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
           :exclude "~/Dropbox/Org/journal"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/html/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("website" :components ("orgfiles" "blog-static"))))
  (setq indent-guide-global-mode t)
  (setq org-export-date-timestamp-format "%Y-%m-%d")
  (setq org-twbs-postamble 't)
  (setq org-twbs-postamble-format
        '(("en" "<div id='disqus_thread'>
            </div>
            <script type='text/javascript'>
                var disqus_shortname = 'blogfreeman';
                (function() {
                    var dsq = document.createElement('script'); dsq.type = 'text/javascript'; dsq.async = true;
                    dsq.src = 'https://' + disqus_shortname + '.disqus.com/embed.js';
                    (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
                })();
            </script>
            <p class='author'>
              Author: %a (%e)
            </p>
            <p>Exported At %T. Created by %c </p>
            <a href='#' class='back-to-top' id='fixed-back-to-top' style='display: inline;'></a>"
           )))
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

  (setq org-babel-python-command "python3")
  (setq python-shell-interpreter "python3")
  (setq org-plantuml-jar-path "~/Dropbox/plantuml.jar")
  (with-eval-after-load 'org
    (setq org-confirm-babel-evaluate nil)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((js . t)
       (emacs-lisp . t)
       (ditaa . t)
       (python . t)
       (plantuml . t)
       (gnuplot . t)
       (haskell . t)
       (shell . t)
       (scala . t)
       (dot . t)
       ))
    (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
    )
  (setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0/libexec/ditaa-0.11.0-standalone.jar")
  (setq org-agenda-persistent-filter t)
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
   '(web-mode tide erc-hl-nicks auctex-latexmk counsel flycheck company ivy magit cider avy org-plus-contrib yasnippet-snippets yaml-mode xterm-color ws-butler winum which-key web-beautify wakatime-mode volatile-highlights vmd-mode vi-tilde-fringe uuidgen use-package typescript-mode treemacs-projectile treemacs-evil toc-org tagedit symon swiper sunshine string-inflection sql-indent spaceline-all-the-icons smeargle slim-mode slack shell-pop shakespeare-mode scss-mode sayid sass-mode reveal-in-osx-finder restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters queue pyim pug-mode popwin plantuml-mode phpunit phpcbf php-extras php-auto-yasnippets persp-mode pcre2el pbcopy password-generator paradox pangu-spacing ox-twbs ox-reveal ox-hugo ox-gfm overseer osx-trash osx-dictionary orgit org-ref org-projectile org-present org-pomodoro org-mime org-journal org-download org-bullets org-brain open-junk-file noflet nameless mvn multi-term mu4e-maildirs-extension mu4e-alert move-text molokai-theme mmm-mode meghanada maven-test-mode markdown-toc magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode linum-relative link-hint less-css-mode launchctl json-mode js2-refactor js-doc intero indent-guide impatient-mode idris-mode hungry-delete hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-purpose helm-projectile helm-mu helm-mode-manager helm-make helm-hoogle helm-gtags helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets groovy-mode groovy-imports gradle-mode google-translate golden-ratio gnuplot github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist ghub gh-md ggtags fuzzy font-lock+ flyspell-popup flyspell-correct-helm flycheck-pos-tip flycheck-haskell flx-ido find-by-pinyin-dired fill-column-indicator fasd fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-ediff evil-cleverparens evil-args evil-anzu eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image ensime emoji-cheat-sheet-plus emmet-mode elisp-slime-nav editorconfig dumb-jump drupal-mode diminish diff-hl dash-at-point dante csv-mode counsel-projectile company-web company-tern company-statistics company-quickhelp company-php company-ghci company-ghc company-emoji company-emacs-eclim company-cabal company-auctex column-enforce-mode color-identifiers-mode coffee-mode cmm-mode clojure-snippets clojure-cheatsheet clj-refactor clean-aindent-mode cider-eval-sexp-fu centered-cursor-mode browse-at-remote auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adaptive-wrap ace-pinyin ace-link ace-jump-helm-line ac-ispell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
