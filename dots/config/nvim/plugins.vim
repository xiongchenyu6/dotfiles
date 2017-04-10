"" Freeman Nvim Settings for back up
"  __ _       _
" / _/__   __(_)_ __ ___
"| |_|\ \ / /| | '_ ` _ \
"|  _| \ V / | | | | | | |
"|_| |  \_/  |_|_| |_| |_|


"*****************************************************************************
"" Vim-PLug core
"*****************************************************************************
" check whether vim-plug is installed and install it if necessary
let plugpath = expand('<sfile>:p:h'). '/autoload/plug.vim'
if !filereadable(plugpath)
    if executable('curl')
        let plugurl = 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
        call system('curl -fLo ' . shellescape(plugpath) . ' --create-dirs ' . plugurl)
        autocmd VimEnter * PlugInstall
        if v:shell_error
            echom "Error downloading vim-plug. Please install it manually.\n"
            exit
        endif
    else
        echom "vim-plug not installed. Please install it manually or install curl.\n"
        exit
    endif
endif

call plug#begin('~/.config/nvim/plugged')

"*****************************************************************************
"" Plug install packages
"*****************************************************************************

"" nerd tree & make nerd tree more like IDE
Plug 'Shougo/unite.vim' | Plug 'Shougo/vimfiler.vim'

"" github plugin
Plug 'tpope/vim-fugitive' | Plug 'airblade/vim-gitgutter'

"" comment
Plug 'scrooloose/nerdcommenter'

"" Fast search
Plug 'ctrlpvim/ctrlp.vim'
Plug 'mhinz/vim-grepper'

"Plug 'bronson/vim-trailing-whitespace'
Plug 'majutsushi/tagbar'
Plug 'Yggdroot/indentLine'
Plug 'tpope/vim-surround'
Plug 'w0rp/ale'
Plug 'Raimondi/delimitMate'
""A collection of language packs for Vim
Plug 'sheerun/vim-polyglot'

"" Vim-Session
Plug 'xolox/vim-misc'
Plug 'xolox/vim-session'

"" Color and theme
Plug 'rafi/awesome-vim-colorschemes'
Plug 'itchyny/lightline.vim'
Plug 'taohex/lightline-buffer'

Plug 'Shougo/vimproc.vim', {'do' : 'make'}

""Auto-complete
Plug 'Shougo/neocomplete.vim' | Plug 'Shougo/neosnippet' | Plug 'Shougo/neosnippet-snippets'

"*****************************************************************************
"" Custom bundles
"*****************************************************************************

Plug 'marijnh/tern_for_vim'
"Plug 'Raimondi/delimitMate'

""Auto-complete for haskell
Plug 'eagletmt/ghcmod-vim' | Plug 'eagletmt/neco-ghc'

Plug 'junegunn/vim-easy-align'

"REPL
Plug 'metakirby5/codi.vim'

"Emmet"
Plug 'mattn/emmet-vim'

"" Color Bundle for css3
Plug 'gorodinskiy/vim-coloresque'

"" Javascript anguler react ..
Plug 'othree/javascript-libraries-syntax.vim'

call plug#end()
"*****************************************************************************

