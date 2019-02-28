call plug#begin('~/.config/nvim/plugged')

"*****************************************************************************
"" Plug install packages
"*****************************************************************************
""Nerd Tree
Plug 'scrooloose/nerdtree' | Plug 'jistr/vim-nerdtree-tabs' | Plug 'Xuyuanp/nerdtree-git-plugin'

"" Fast search and Most Recently used
Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }

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

Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'

"*****************************************************************************
"" Custom bundles
"*****************************************************************************

"Emmet for zen coding
Plug 'mattn/emmet-vim'

"" Color Bundle for css3
Plug 'gorodinskiy/vim-coloresque'

Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': './install.sh'
    \ }

Plug 'junegunn/fzf'

Plug 'wakatime/vim-wakatime'

call plug#end()
