;;; init.el -*- lexical-binding: t; -*-
;; Copy me to ~/.doom.d/init.el or ~/.config/doom/init.el, then edit me!
(setq doom-localleader-key ",")

(doom! :feature

       :completion
       (company                         ; the ultimate code completion backend
        +childframe) ; as-you-type code completion                    ; a nicer company UI (Emacs 26+ only)
       ;; helm            ; the *other* search engine for love and life
       ;; ido            ; the other *other* search engine...
       (ivy +fuzzy +childframe +icons +prescient)

       :ui
       deft                             ; notational velocity for Emacs
       doom
       doom-dashboard
       doom-quit                 ; DOOM quit-message prompts when you quit Emacs
       fill-column               ; a `fill-column' indicator
       hl-todo                   ; highlight TODO/FIXME/NOTE tags
       modeline                  ; snazzy, Atom-inspired modeline, plus API
       indent-guides
       nav-flash   ; blink the current line after jumping
       ;;neotree           ; a project drawer, like NERDTree for vim
       treemacs
                                        ; a project drawer, like neotree but cooler
       (popup                     ; tame sudden yet inevitable temporary windows
        +all                      ; catch all popups that start with an asterix
        +defaults)
                                        ; default popup rules
       (pretty-code +fira)            ; replace bits of code with pretty symbols
        ;;   tabs
                                        ; FIXME an (incomplete) tab bar for Emacs
                                        ; unicode           ; extended unicode support for various languages
       vc-gutter                        ; vcs diff in the fringe
       vi-tilde-fringe                  ; fringe tildes to mark beyond EOB
       workspaces             ; tab emulation, persistence & separate workspaces
       tabs
       window-select          ; visually switch windows

       :editor
       evil                 ; come to the dark side, we have cookies
       file-templates       ; auto-snippets for empty files
       fold                 ; (nigh) universal code folding
       (format +onsave)     ; automated prettiness
       lispy                ; vim for lisp, for people who dont like vim
       multiple-cursors     ; editing in many places at once
       ;;parinfer          ; turn lisp into python, sort of
       snippets                  ; my elves. They type so I don't have to
       rotate-text               ; cycle region at point between text candidates
       word-wrap

       :emacs
       (dired                         ; making dired pretty [functional]
        +ranger                       ; bringing the goodness of ranger to dired
        +icons                        ; colorful icons for dired-mode
        )
       electric                   ; smarter, keyword-based electric-indent
       vc                         ; version-control and Emacs, sitting in a tree

       :term                          ; terminals in Emacs
       term                         ; a consistent, cross-platform shell (WIP)

       :tools
       ;;ansible
       docker
       debugger          ; FIXME stepping through code, to help you add bugs
       editorconfig      ; let someone else argue about tabs vs spaces
       eval              ; run code, run (also, repls)
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       ;;ein               ; tame Jupyter notebooks with emacs
       flycheck                      ; tasing you for every semicolon you forget
       flyspell                      ; tasing you for misspelling mispelling
       gist                          ; interacting with github gists
       lsp
       ;;macos             ; MacOS-specific commands
       magit             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       pass              ; password manager for nerds
       pdf               ; pdf enhancements
       prodigy           ; FIXME managing external services & code builders
       rgb               ; creating color strings
       ;;terraform         ; infrastructure as code
       ;; tmux              ; an API for interacting with tmux
       upload                         ; map local to remote projects via ssh/ftp
       wakatime
       ;; vterm             ; another terminals in Emacs

       :lang
       assembly                         ; assembly for fun or debugging
       agda
       (cc +lsp)                        ; C/C++/Obj-C madness
       clojure                          ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data   ; config/data formats
       ;;erlang            ; an elegant language for a more civilized age
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp                  ; drown in parentheses
       ;;ess               ; emacs speaks statistics
       ;;go                ; the hipster dialect
       (haskell +intero ;; +lsp
                )                   ; a language that's lazier than I am
       ;; ;;hy                ; readability of scheme w/ speed of python
       idris             ;
       ;; ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       ;; javascript
                                        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;latex          ; writing papers in Emacs has never been so fun
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org                             ; organize your plain life in plain text
        +dragndrop
        +gnuplot
        +pandoc
        +present)
                                        ; Emacs for presentations
       ;;perl              ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       plantuml            ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python +lsp +pyenv)       ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       rest                             ; Emacs as a REST client
       ;;ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       (scala +lsp)         ; java, but good
       sh              ; she sells (ba|z|fi)sh shells on the C xor
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       web                              ; the tubes
       ;;vala              ; GObjective-C

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :email
       (mu4e +gmail)                    ; emacs as an email client
       :app
       ;;irc               ; how neckbeards socialize
       (rss +org)                       ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought
       (write               ; emacs as a word processor (latex + org + markdown)
        +wordnut            ; wordnet (wn) search
        +langtool)          ; a proofreader (grammar/style check) for Emacs

       :collab
       ;;floobits          ; peer programming for a price
       ;;impatient-mode    ; show off code over HTTP

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       ;;literate

       ;; The default module sets reasonable defaults for Emacs. It also
       ;; provides a Spacemacs-inspired keybinding scheme and a smartparens
       ;; config. Use it as a reference for your own modules.
       (default +bindings +smartparens)
       :private
       my-leet-code
       )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b0fd04a1b4b614840073a82a53e88fe2abc3d731462d6fde4e541807825af342" "e3c87e869f94af65d358aa279945a3daf46f8185f1a5756ca1c90759024593dd" "868abc288f3afe212a70d24de2e156180e97c67ca2e86ba0f2bf9a18c9672f07" "155a5de9192c2f6d53efcc9c554892a0d87d87f99ad8cc14b330f4f4be204445" default))
 '(safe-local-variable-values '((checkdoc-package-keywords-flag)))
 '(wakatime-api-key "06fb08d0-68a4-4b39-bbb0-d34d325dc046")
 '(wakatime-cli-path "/usr/bin/wakatime")
 '(wakatime-python-bin nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
