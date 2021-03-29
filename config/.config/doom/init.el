;;; init.el -*- lexical-binding: t; -*-
;; Copy me to ~/.doom.d/init.el or ~/.config/doom/init.el, then edit me!
(setq doom-localleader-key ",")

(doom! :feature

       :completion
       (company                         ; the ultimate code completion backend
        +childframe
        ) ; as-you-type code completion                    ; a nicer company UI (Emacs 26+ only)
       ;; helm            ; the *other* search engine for love and life
       ;; ido            ; the other *other* search engine...
       (ivy
        +childframe
        +icons +prescient)

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
       ophints     ; highlight the region an operation acts on
       ;;neotree           ; a project drawer, like NERDTree for vim
       treemacs
                                        ; a project drawer, like neotree but cooler
       (popup                     ; tame sudden yet inevitable temporary windows
        +all                      ; catch all popups that start with an asterix
        +defaults)

       (ligatures +extra)
                                        ; FIXME an (incomplete) tab bar for Emacs
                                        ; unicode           ; extended unicode support for various languages
       vc-gutter                        ; vcs diff in the fringe
       vi-tilde-fringe                  ; fringe tildes to mark beyond EOB
       workspaces             ; tab emulation, persistence & separate workspaces
       tabs
       window-select                    ; visually switch windows

       :editor
       evil                 ; come to the dark side, we have cookies
       file-templates       ; auto-snippets for empty files
       fold                 ; (nigh) universal code folding
       (format +onsave)     ; automated prettiness
       lispy                ; vim for lisp, for people who dont like vim
       multiple-cursors
                                        ; editing in many places at once
       snippets                  ; my elves. They type so I don't have to
       rotate-text               ; cycle region at point between text candidates
       word-wrap

       :emacs
       (dired                         ; making dired pretty [functional]
        +ranger                       ; bringing the goodness of ranger to dired
        +icons                        ; colorful icons for dired-mode
        )
       electric                         ; smarter, keyword-based electric-indent
       (ibuffer +icons)
       vc                         ; version-control and Emacs, sitting in a tree

       :term                          ; terminals in Emacs
       ;; (term)
                                        ; a consistent, cross-platform shell (WIP)
       :checkers
       syntax                        ; tasing you for every semicolon you forget
       (spell +flyspell)                         ; tasing you for misspelling mispelling
       grammar                       ; tasing grammar mistake every you make
       :input
       chinese

       :tools
       ansible
       docker
       (debugger +lsp)       ; FIXME stepping through code, to help you add bugs
       direnv
       editorconfig          ; let someone else argue about tabs vs spaces
       (eval +overlay)       ; run code, run (also, repls)
       (lookup               ; helps you navigate your code and documentation
        +xwidget
        +dictionary
        +offline
        +docsets)
       ;;ein               ; tame Jupyter notebooks with emacs
       gist                             ; interacting with github gists
       (lsp +peek)
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

       :lang
       ;; agda
       (cc +lsp)                        ; C/C++/Obj-C madness
       (clojure +lsp)                          ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;; coq    ; proofs-as-programs
       ;;crystal          ; ruby at the speed of c
       data   ; config/data formats
       ;;erlang            ; an elegant language for a more civilized age
       ;;elixir            ; erlang done right
       emacs-lisp                  ; drown in parentheses
       ;;ess               ; emacs speaks statistics
       (go +lsp)                        ; the hipster dialect
       (haskell +lsp)                   ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;; idris             ;
       ; (java +lsp) ; the poster child for carpal tunnel syndrome
       json
       javascript ;; +lsp

                                        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;latex          ; writing papers in Emacs has never been so fun
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       (markdown +grip)    ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       nix                              ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org                             ; organize your plain life in plain text
        +dragndrop
        +gnuplot
        ;; +pandoc
        +pomodoro
        +gnuplot
        +pretty
        +present)
                                        ; Emacs for presentations
       ;;perl              ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       plantuml                 ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python +lsp +pyenv)             ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       rest      ; Emacs as a REST client
       (rust +lsp)              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       (scala +lsp)         ; java, but good
       (sh +lsp)            ; she sells (ba|z|fi)sh shells on the C xor
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       (web +html +css)                 ; the tubes
       ;;vala              ; GObjective-C
       yaml

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :email
       (mu4e +gmail)                    ; emacs as an email client
       :app
       calendar
       irc                              ; how neckbeards socialize
       (rss +org)                       ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

                                        ; a proofreader (grammar/style check) for Emacs

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
