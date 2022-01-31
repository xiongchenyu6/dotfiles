"" Freeman Nvim Settings for back up
"  __ _       _
" / _/__   __(_)_ __ ___
"| |_|\ \ / /| | '_ ` _ \
"|  _| \ V / | | | | | | |
"|_| |  \_/  |_|_| |_| |_|


"*****************************************************************************
"" Basic Setup
"*****************************************************************************"

scriptencoding utf-8

" Identify platform {
let g:MAC = has('macunix')
let g:LINUX = has('unix') && !has('macunix') && !has('win32unix')
let g:WINDOWS = has('win32') || has('win64')
" }

" Windows Compatible {
" On Windows, also use '.vim' instead of 'vimfiles'; this makes synchronization
" across (heterogeneous) systems easier.
if g:WINDOWS
    set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
endif
" }

let g:vim_dir = $HOME.'/.config/nvim'

set runtimepath+=$HOME/.config/nvim

source $HOME/.config/nvim/plugins.vim

"" Tabs. May be overriten by autocmd rules
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab

"" Map leader to ,
nnoremap <SPACE> <Nop>
let mapleader=' '

"" Enable hidden buffers
set hidden
set showtabline=2
" Set 7 lines to the cursor - when moving vertically using j/k
set so=7

"" Searching
set ignorecase
set smartcase
" Show matching brackets when text indicator is over them
set showmatch
" How many tenths of a second to blink when matching brackets
set mat=2

"" Directories for swp files
set nobackup
set nowb
set noswapfile

set fileformats=unix,dos,mac

filetype on
filetype plugin indent on

"*****************************************************************************
"" Visual Settings
"*****************************************************************************
set number
set spell
"set foldenable
set foldmethod=syntax
set foldlevelstart=99

" For regular expressions turn magic on
set magic

" 相对行号: 行号变成相对，可以用 nj/nk 进行跳转
set relativenumber number

"Save on buffer switch
set autowrite

""Change cursor when change to insert mode
let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 1
"" Use modeline overrides
set modeline
set modelines=10

set termguicolors
colorscheme molokai
hi! Normal ctermbg=NONE guibg=NONE
hi! NonText ctermbg=NONE guibg=NONE

"*****************************************************************************
"" Light Line Settings
"*****************************************************************************
let g:lightline = {
			\ 'colorscheme': 'molokai',
			\ 'active': {
			\   'left': [ [ 'mode', 'paste' ], [ 'readonly', 'fugitive', 'gitgutter', 'ale', 'filename' ] ],
			\   'right': [ [ 'lineinfo' ], ['percent'], [ 'filetype', 'fileformat', 'fileencoding' ] ]
			\ },
			\ 'component': {
			\   'spell': '%{&spell?&spelllang:"no spell"}',
			\ },
			\ 'component_visible_condition': {
			\   'readonly': '(&filetype!="help"&& &readonly)',
			\ },
			\ 'component_function': {
			\   'fugitive': 'LightlineFugitive',
			\   'gitgutter': 'LightlineGutter',
			\   'ale' : 'LightlineAle',
			\   'filename': 'LightlineFilename',
			\   'fileformat': 'LightlineFileformat',
			\   'filetype': 'LightlineFiletype',
			\   'fileencoding': 'LightlineFileencoding',
			\   'mode': 'LightlineMode',
			\ 'bufferbefore': 'lightline#buffer#bufferbefore',
			\ 'bufferafter': 'lightline#buffer#bufferafter',
			\ 'bufferinfo': 'lightline#buffer#bufferinfo',
			\ },
			\ 'component_expand': {
			\ 'buffercurrent': 'lightline#buffer#buffercurrent2',
			\ },
			\ 'component_type': {
			\ 'buffercurrent': 'tabsel',
			\ },
			\ 'separator': { 'left': "\ue0b0", 'right': "\ue0b2" },
			\ 'subseparator': { 'left': "\ue0b1", 'right': "\ue0b3" }
			\ }

function! LightlineModified()
	return &ft =~ 'help' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction

function! LightlineReadonly()
	return &ft !~? 'help' && &readonly ? 'RO' : ''
endfunction

function! LightlineFilename()
	let fname = expand('%:t')
	return fname == 'ControlP' && has_key(g:lightline, 'ctrlp_item') ? g:lightline.ctrlp_item :
				\ fname == '__Tagbar__' ? g:lightline.fname :
				\ fname =~ '__Gundo\|NERD_tree' ? '' :
				\ &ft == 'vimfiler' ? vimfiler#get_status_string() :
				\ &ft == 'unite' ? unite#get_status_string() :
				\ &ft == 'vimshell' ? vimshell#get_status_string() :
				\ ('' != LightlineReadonly() ? LightlineReadonly() . ' ' : '') .
				\ ('' != LightlineModified() ? ' ' . LightlineModified() : '')
endfunction

function! LightlineAle()
	let fname = expand('%:t')
	return &ft == 'vimfiler' ? '' :
				\ fname == '__Tagbar__' ? '' : ''

endfunction

function! LightlineFugitive()
	try
		if expand('%:t') !~? 'Tagbar\|Gundo\|NERD' && &ft !~? 'vimfiler' && exists('*fugitive#head')
			let mark = ' '  " edit here for cool mark
			let branch = fugitive#head()
			return branch !=# '' ? mark.branch : ''
		endif
	catch
	endtry
	return ''
endfunction

function! LightlineGutter()
	if ! exists('*GitGutterGetHunkSummary')
				\ || ! get(g:, 'gitgutter_enabled', 0)
				\ || winwidth('.') <= 90
		return ''
	endif
	let symbols = [
				\ g:gitgutter_sign_added . ' ',
				\ g:gitgutter_sign_modified . ' ',
				\ g:gitgutter_sign_removed . ' '
				\ ]
	let hunks = GitGutterGetHunkSummary()
	let ret = []
	for i in [0, 1, 2]
		if hunks[i] > 0
			call add(ret, symbols[i] . hunks[i])
		endif
	endfor
	return join(ret, ' ')
endfunction

function! LightlineFileformat()
	return winwidth(0) > 70 ? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()) : ''
endfunction

function! LightlineFiletype()
	return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ''
endfunction

function! LightlineFileencoding()
	return winwidth(0) > 70 ? (&fenc !=# '' ? &fenc : &enc) : ''
endfunction

function! LightlineMode()
	let fname = expand('%:t')
	return fname == '__Tagbar__' ? 'Tagbar' :
				\ fname == 'ControlP' ? 'CtrlP' :
				\ fname =~ 'NERD_tree' ? 'NERDTree' :
				\ fname == 'denite' ? denite#get_status_mode() :
				\ winwidth(0) > 60 ? lightline#mode() : ''
endfunction

let g:tagbar_status_func = 'TagbarStatusFunc'

function! TagbarStatusFunc(current, sort, fname, ...) abort
	let g:lightline.fname = a:fname
	return lightline#statusline(0)
endfunction

let g:ale_statusline_format = ['✘ %d', '⚠ %d', '⬥ ok']
let g:ale_open_list = 0

" vim-gitgutter
let g:gitgutter_sign_added ='✚'
let g:gitgutter_sign_modified ='➜'
let g:gitgutter_sign_removed ='✘'

" lightline-buffer ui settings
" replace these symbols with ascii characters if your environment does not support unicode
let g:lightline_buffer_logo = ' '
let g:lightline_buffer_readonly_icon = ''
let g:lightline_buffer_modified_icon = '✭'
let g:lightline_buffer_git_icon = ' '
let g:lightline_buffer_ellipsis_icon = '..'
let g:lightline_buffer_expand_left_icon = '◀ '
let g:lightline_buffer_expand_right_icon = ' ▶'
let g:lightline_buffer_active_buffer_left_icon = ''
let g:lightline_buffer_active_buffer_right_icon = ''
let g:lightline_buffer_separator_icon = ' '

" lightline-buffer function settings
let g:lightline_buffer_show_bufnr = 1
let g:lightline_buffer_rotate = 0
let g:lightline_buffer_fname_mod = ':t'
let g:lightline_buffer_excludes = ['vimfiler']

let g:lightline_buffer_maxflen = 30
let g:lightline_buffer_maxfextlen = 3
let g:lightline_buffer_minflen = 16
let g:lightline_buffer_minfextlen = 3
let g:lightline_buffer_reservelen = 20

"*****************************************************************************
"" Abbreviations
"*****************************************************************************

" Denite --------------------------------------------------------------------{{{
"
call denite#custom#option('default', 'prompt', '❯')
"  \     'rg', '--glob', '!.git', ''

call denite#custom#source(
			\ 'file_rec', 'vars', {
			\   'command': [
			\      'pt', '--follow','--nogroup','--hidden'
			\   ] })

" Ag command on grep source
call denite#custom#var('grep', 'command', ['pt'])
call denite#custom#var('grep', 'default_opts',
			\ ['-i'])
call denite#custom#var('grep', 'recursive_opts', [])
call denite#custom#var('grep', 'pattern_opt', [])
call denite#custom#var('grep', 'separator', ['--'])
call denite#custom#var('grep', 'final_opts', [])

" call denite#custom#source('file_rec', 'sorters', ['sorter_sublime'])
" call denite#custom#option('default', 'statusline', 0)
call denite#custom#option('default', 'highlight-matched-char', '')
call denite#custom#option('default', 'highlight-matched-range', '')
hi deniteMatched guibg=None
hi deniteMatchedChar guibg=None

nnoremap <silent> <c-p> :FZF<CR>
nnoremap <silent> <leader>b :Denite buffer<CR>
nnoremap <silent> <Leader>s : Denite line<CR>
nnoremap <Leader>g :<C-u>Denite -auto-resize menu:git<CR>
nnoremap <leader>f :<C-u>DeniteBufferDir file_rec<CR>
nnoremap <leader>8 :<C-u>DeniteCursorWord grep:. -mode=normal<CR>
nnoremap <leader>/ :<C-u>DeniteBufferDir grep:. -mode=normal<CR>

let s:menus = {}

call denite#custom#map(
			\ 'insert',
			\ '<C-n>',
			\ '<denite:move_to_next_line>',
			\ 'noremap'
			\)
call denite#custom#map(
			\ 'insert',
			\ '<C-p>',
			\ '<denite:move_to_previous_line>',
			\ 'noremap'
			\)

call denite#custom#filter('matcher_ignore_globs', 'ignore_globs',
			\ [ '.git/', '.meteor/', '.ropeproject/', '__pycache__/',
			\   'venv/', 'images/', '*.min.*', 'img/', 'fonts/'])

call denite#custom#var('menu', 'menus', s:menus)

"}}}

" Git from denite...ERMERGERD -----------------------------------------------{{{
let s:menus.git = {
			\ 'description' : 'Fugitive interface',
			\}
let s:menus.git.command_candidates = [
			\[' git status', 'Gstatus'],
			\[' git diff', 'Gvdiff'],
			\[' git commit', 'Gcommit'],
			\[' git stage/add', 'Gwrite'],
			\[' git checkout', 'Gread'],
			\[' git rm', 'Gremove'],
			\[' git cd', 'Gcd'],
			\[' git push', 'exe "Git! push " input("remote/branch: ")'],
			\[' git pull', 'exe "Git! pull " input("remote/branch: ")'],
			\[' git pull rebase', 'exe "Git! pull --rebase " input("branch: ")'],
			\[' git checkout branch', 'exe "Git! checkout " input("branch: ")'],
			\[' git fetch', 'Gfetch'],
			\[' git merge', 'Gmerge'],
			\[' git browse', 'Gbrowse'],
			\[' git head', 'Gedit HEAD^'],
			\[' git parent', 'edit %:h'],
			\[' git log commit buffers', 'Glog --'],
			\[' git log current file', 'Glog -- %'],
			\[' git log last n commits', 'exe "Glog -" input("num: ")'],
			\[' git log first n commits', 'exe "Glog --reverse -" input("num: ")'],
			\[' git log until date', 'exe "Glog --until=" input("day: ")'],
			\[' git log grep commits',  'exe "Glog --grep= " input("string: ")'],
			\[' git log pickaxe',  'exe "Glog -S" input("string: ")'],
			\[' git index', 'exe "Gedit " input("branchname\:filename: ")'],
			\[' git mv', 'exe "Gmove " input("destination: ")'],
			\[' git grep',  'exe "Ggrep " input("string: ")'],
			\[' git prompt', 'exe "Git! " input("command: ")'],
			\] " Append ' --' after log to get commit info commit buffers
"}}}

"" NERDTree configuration
let g:NERDTreeChDirMode=2
let g:NERDTreeIgnore=['\.rbc$', '\~$', '\.pyc$', '\.db$', '\.sqlite$', '__pycache__']
let g:NERDTreeSortOrder=['^__\.py$', '\/$', '*', '\.swp$', '\.bak$', '\~$']
let g:NERDTreeShowBookmarks=1
let g:nerdtree_tabs_focus_on_files=1
let g:NERDTreeMapOpenInTabSilent = '<RightMouse>'
let NERDTreeKeepTreeInNewTab=1

map <silent> <leader>ft <plug>NERDTreeTabsToggle<CR>
nmap <silent> <leader>nt :NERDTreeFind<CR>

" session management
let g:session_directory = vim_dir."/session"
let g:session_autoload = "no"
let g:session_autosave = "no"
let g:session_command_aliases = 1

" Enable persistent undo so that undo history persists across vim sessions
set undofile
set undodir=~/.vim/undo
nmap <silent> <leader>u :MundoToggle<CR>

"" Tabs
nnoremap <Tab> gt
nnoremap <S-Tab> gT
nnoremap <silent> <S-t> :tabnew<CR>

"Tmux
" Write all buffers before navigating from Vim to tmux pane
let g:tmux_navigator_save_on_switch = 2

"*****************************************************************************
"" Functions
"*****************************************************************************
function s:setupWrapping()
	set wrap
	set wm=2
	set textwidth=79
endfunction

"*****************************************************************************
"" Autocmd Rules
"*****************************************************************************
"" Remember cursor position
augroup vimrc-remember-cursor-position
	autocmd!
	autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
augroup END

"" txt
augroup vimrc-wrapping
	autocmd!
	autocmd BufRead,BufNewFile *.txt call s:setupWrapping()
augroup END

"" make/cmake
augroup vimrc-make-cmake
	autocmd!
	autocmd FileType make setlocal noexpandtab
	autocmd BufNewFile,BufRead CMakeLists.txt setlocal filetype=cmake
augroup END

set autoread

"*****************************************************************************
"" Mappings
"*****************************************************************************
nnoremap <expr> k (v:count == 0 ? 'gk' : 'k')
nnoremap <expr> j (v:count == 0 ? 'gj' : 'j')

"" Split
noremap <Leader>ws :<C-u>split<CR>
noremap <Leader>wv :<C-u>vsplit<CR>

" session management
"nnoremap <leader>so :OpenSession<Space>
"nnoremap <leader>ss :SaveSession<Space>
"nnoremap <leader>sd :DeleteSession<CR>
"nnoremap <leader>sc :CloseSession<CR>

" Tagbar
map <Leader>tt :TagbarToggle<CR>
let g:tagbar_autofocus = 1

"" Copy/Paste/Cut
set clipboard=unnamed

set pastetoggle=<F2>

"" Buffer nav
noremap <leader>z :bp<CR>
noremap <leader>x :bn<CR>
noremap [b :bp<CR>
noremap ]b :bn<CR>

"" Close buffer
noremap <leader>bx :bd<CR>

"" Clean search (highlight)
nnoremap <silent> <C-g> :noh<cr>

" Search mappings: These will make it so that going to the next one in a
" search will center on the line it's found in.
nnoremap n nzzzv
nnoremap N Nzzzv

" 插入模式下用绝对行号, 普通模式下用相对
autocmd InsertEnter * :set norelativenumber number
autocmd InsertLeave * :set relativenumber

" 调整缩进后自动选中，方便再次操作
vnoremap < <gv
vnoremap > >gv

"syntastic

nmap <silent> <[-e> <Plug>(ale_previous_wrap)
nmap <silent> <]-e> <Plug>(ale_next_wrap)

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

"*****************************************************************************
"" Custom command maps
"*****************************************************************************

let g:indentLine_setConceal = 0
" Change Working Directory to that of the current file
cmap cd. lcd %:p:h

" For when you forget to sudo.. Really Write the file.
cmap w!! w !sudo tee % >/dev/null

"*****************************************************************************
"" Auto complete configuration
"*************************************************************************pip3 install --upgrade neovip3 install --upgrade neovimm****

" enable ncm2 for all buffers
autocmd BufEnter * call ncm2#enable_for_buffer()

" IMPORTANT: :help Ncm2PopupOpen for more information
set completeopt=noinsert,menuone,noselect

let g:UltiSnipsExpandTrigger="<C-j>"

set shortmess+=c

inoremap <expr> <CR> (pumvisible() ? "\<c-y>\<cr>" : "\<CR>")

let g:LanguageClient_serverCommands = {
      \ 'c': ['clangd'],
      \ 'cpp': ['clangd'],
      \ 'objc': ['clangd'],
      \ 'haskell': ['haskell-language-server-wrapper'],
      \ 'scala': ['metals-vim'],
      \ 'go': ['gopls'],
      \ 'rust': ['rust-analyzer'],
      \ }

map <Leader>lk :call LanguageClient#textDocument_hover()<CR>
map <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
map <Leader>lb :call LanguageClient#textDocument_references()<CR>
map <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
map <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>

nnoremap <F5> :call LanguageClient_contextMenu()<CR>
" Or map each action separately
nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>

let g:ale_linters = { 
      \ 'haskell': ['haskell-language-server-wrapper'],
      \ 'cpp': ['clangd'],
          \ }
let g:ale_sign_column_always = 1
let g:ale_completion_enabled = 1
" close the preview window when you're not using it
let g:SuperTabClosePreviewOnPopupClose = 1

	"*****************************************************************************
	"" Self Customise
	"*****************************************************************************
let g:WebDevIconsOS = 'Darwin'

set noimdisable
autocmd! InsertLeave * set imdisable|set iminsert=0
autocmd! InsertEnter * set noimdisable|set iminsert=0
