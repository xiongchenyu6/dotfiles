"*****************************************************************************
"" Vim-PLug core
"*****************************************************************************
" check whether vim-plug is installed and install it if necessary
let plugpath = expand('<sfile>:p:h'). '~/.config/nvim/autoload/plug.vim'
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
""Nerd Tree
Plug 'scrooloose/nerdtree' | Plug 'jistr/vim-nerdtree-tabs' | Plug 'Xuyuanp/nerdtree-git-plugin'

"" Fast search and Most Recently used
Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/neomru.vim'

"" github plugin
Plug 'tpope/vim-fugitive' | Plug 'airblade/vim-gitgutter' | Plug 'tpope/vim-rhubarb'

"" comment
Plug 'scrooloose/nerdcommenter'

"" Tags bar and auto tags update
Plug 'majutsushi/tagbar'
Plug 'xolox/vim-easytags'

"" Show indentLine
Plug 'Yggdroot/indentLine'
Plug 'tpope/vim-surround'

""Syntactic check
Plug 'w0rp/ale'
Plug 'Raimondi/delimitMate'

"" Vim-Session
Plug 'xolox/vim-misc'
Plug 'xolox/vim-session'

"" Color theme powerline syntax highlight and icons
Plug 'rafi/awesome-vim-colorschemes'
Plug 'itchyny/lightline.vim'
Plug 'taohex/lightline-buffer'
Plug 'ryanoasis/vim-devicons'
""better poly syntax
Plug 'sheerun/vim-polyglot'

"" Utils
Plug 'Shougo/vimproc.vim', {'do' : 'make'}
Plug 'Raimondi/delimitMate'
Plug 'junegunn/vim-easy-align'
Plug 'metakirby5/codi.vim'
Plug 'vimwiki/vimwiki'
Plug 'mhinz/vim-startify'
Plug 'mattn/calendar-vim'
Plug 'simnalamburt/vim-mundo'

""Tmux compatibility
Plug 'christoomey/vim-tmux-navigator'
Plug 'roxma/vim-tmux-clipboard'

""Auto-complete
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

"*****************************************************************************
"" Custom bundles
"*****************************************************************************

""Auto-complete for haskell
Plug 'myfreeweb/intero.nvim'

""Auto-complete for Shell
Plug 'Shougo/vimshell.vim'

"" WEB DEVELOPMENT
""Auto-complete for Javascript
Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }
Plug 'othree/jspc.vim'

"Emmet for zen coding
Plug 'mattn/emmet-vim'

"" Color Bundle for css3
Plug 'gorodinskiy/vim-coloresque'

Plug 'wakatime/vim-wakatime'

call plug#end()
