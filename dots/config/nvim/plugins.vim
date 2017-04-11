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

"" Fast search
"" nerd tree & make nerd tree more like IDE
Plug 'Shougo/unite.vim' | Plug 'Shougo/vimfiler.vim'

""Most Recently used
Plug 'Shougo/neomru.vim'

"" github plugin
Plug 'tpope/vim-fugitive' | Plug 'airblade/vim-gitgutter'

"" comment
Plug 'scrooloose/nerdcommenter'

"" Tags 
Plug 'majutsushi/tagbar'
Plug 'xolox/vim-easytags'

Plug 'Yggdroot/indentLine'
Plug 'tpope/vim-surround'
Plug 'w0rp/ale'
Plug 'Raimondi/delimitMate'

"" Vim-Session
Plug 'xolox/vim-misc'
Plug 'xolox/vim-session'

"" Color theme and icons
Plug 'rafi/awesome-vim-colorschemes'
Plug 'itchyny/lightline.vim'
Plug 'taohex/lightline-buffer'
Plug 'ryanoasis/vim-devicons'

Plug 'Shougo/vimproc.vim', {'do' : 'make'}

""Auto-complete
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/neosnippet'
Plug 'Shougo/neosnippet-snippets'
Plug 'honza/vim-snippets'

Plug 'Raimondi/delimitMate'

""Tmux compatibility
Plug 'christoomey/vim-tmux-navigator'
Plug 'roxma/vim-tmux-clipboard'

""better poly syntax
Plug 'sheerun/vim-polyglot'
"*****************************************************************************
"" Custom bundles
"*****************************************************************************

""Auto-complete for Javascript
Plug 'carlitux/deoplete-ternjs', {'do':['npm install -g tern'], 'for': ['javascript', 'javascript.jsx'] }
Plug 'othree/jspc.vim', { 'for': ['javascript', 'javascript.jsx'] }

""Auto-complete for haskell
Plug 'eagletmt/ghcmod-vim' | Plug 'eagletmt/neco-ghc'

Plug 'junegunn/vim-easy-align'

""Auto-complete for Javascript
Plug 'Shougo/vimshell.vim'
"REPL
Plug 'metakirby5/codi.vim'

"Emmet"
Plug 'mattn/emmet-vim'

"" Color Bundle for css3
Plug 'gorodinskiy/vim-coloresque'

call plug#end()
"*****************************************************************************
