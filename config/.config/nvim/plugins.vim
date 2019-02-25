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
Plug 'ryanoasis/vim-devicons'
""better poly syntax
Plug 'sheerun/vim-polyglot'

"" Utils
Plug 'Shougo/vimproc.vim', {'do' : 'make'}
Plug 'Raimondi/delimitMate'
Plug 'junegunn/vim-easy-align'
Plug 'mhinz/vim-startify'
Plug 'mattn/calendar-vim'
Plug 'simnalamburt/vim-mundo'

""Tmux compatibility
Plug 'christoomey/vim-tmux-navigator'
Plug 'roxma/vim-tmux-clipboard'

""Auto-complete
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" assuming you're using vim-plug: https://github.com/junegunn/vim-plug
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'

" NOTE: you need to install completion sources to get completions. Check
" our wiki page for a list of sources: https://github.com/ncm2/ncm2/wiki
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'

"*****************************************************************************
"" Custom bundles
"*****************************************************************************

"" WEB DEVELOPMENT
""Auto-complete for Javascript
Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }

"Emmet for zen coding
Plug 'mattn/emmet-vim'

"" Color Bundle for css3
Plug 'gorodinskiy/vim-coloresque'

Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'

Plug 'wakatime/vim-wakatime'

call plug#end()
