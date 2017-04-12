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
source ~/.config/nvim/plugins.vim

""set boolean
set bomb

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

"" Status bar
set laststatus=2

"" Use modeline overrides
set modeline
set modelines=10

set title
set titleold="Terminal"
set titlestring=%F

set statusline=%F%m%r%h%w%=(%{&ff}/%Y)\ (line\ %l\/%L,\ col\ %c)\

let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

set termguicolors
colorscheme solarized8_dark_high

"*****************************************************************************
"" Light Line Settings
"*****************************************************************************
let g:lightline = {
      \ 'colorscheme': 'solarized',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], [ 'readonly', 'fugitive', 'gitgutter', 'ale', 'filename' ] ],
      \   'right': [ [ 'lineinfo' ], ['percent'], [ 'filetype', 'fileformat', 'fileencoding' ] ]
      \ },
      \ 'tabline': {
      \ 'left': [ [ 'bufferinfo' ], [ 'bufferbefore', 'buffercurrent', 'bufferafter' ], ],
      \ 'right': [ [ 'close' ], ],
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
        \ fname == '__Tagbar__' ? '' :
        \ ALEGetStatusLine()
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

let g:unite_prompt = "➤ "
let g:unite_winheight = 20
let g:unite_split_rule = 'botright'
let g:unite_enable_ignore_case = 1
let g:unite_enable_smart_case = 1
let g:unite_enable_start_insert = 1

let g:unite_source_file_mru_limit = 200
let g:unite_source_history_yank_enable = 1
let g:unite_source_rec_max_cache_files=5000

let g:unite_source_grep_command = 'ag'
let g:unite_source_grep_default_opts = '--line-numbers --nocolor --nogroup --hidden --ignore ' .
      \  '''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'''
let g:unite_source_grep_recursive_opt = ''

" Git from unite_source_grep_recursive_optnite...ERMERGERD -----------------------------------------------{{{
let g:unite_source_menu_menus = get(g:,'unite_source_menu_menus',{})
let g:unite_source_menu_menus.git = {
      \ 'description' : 'Fugitive interface',
      \}
let g:unite_source_menu_menus.git.command_candidates = [
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

nnoremap <C-p> : Unite buffer file_mru file/async file_rec/async<CR>
nnoremap <Leader>f : Unite grep:.<cr>
nnoremap <Leader>u : Unite line -prompt-direction="top"<CR>
nnoremap <Leader>g : Unite -silent -start-insert menu:git<CR>

function! s:unite_settings()
  nmap <buffer> <esc> <plug>(unite_exit)
  imap <buffer> <esc> <plug>(unite_exit)
  imap <silent><buffer> <C-k> <C-p>
  imap <silent><buffer> <C-j> <C-n>
  imap <silent><buffer> <C-d> <CR>
  call unite#filters#matcher_default#use(['matcher_fuzzy'])
  call unite#filters#sorter_default#use(['sorter_rank'])
  call unite#custom#source('file_rec,file_rec/async', 'ignore_pattern', '(\.meta$|\.tmp)')
endfunction

autocmd FileType unite call s:unite_settings()

let g:vimfiler_enable_auto_cd = 1
let g:vimfiler_enable_clipboard = 0
let g:vimfiler_as_default_explorer = 1
let g:vimfiler_safe_mode_by_default = 0
let g:vimfiler_ignore_pattern = '\%(.DS_Store\|.pyc\|.git\w*\|.sw\w*\|.hg\|.svn\)$'
let g:vimfiler_force_overwrite_statusline = 0

let g:vimfiler_tree_leaf_icon = ''
let g:vimfiler_tree_opened_icon = '▾'
let g:vimfiler_tree_closed_icon = '▸'
let g:vimfiler_default_columns = ''
let g:vimfiler_explorer_columns = ''
let g:vimfiler_tree_indentation = 3
let g:vimfiler_file_icon = '·'
let g:vimfiler_marked_file_icon = '✩'
let g:vimfiler_readonly_file_icon = '○'

autocmd FileType vimfiler setlocal nonumber
autocmd FileType vimfiler setlocal norelativenumber
autocmd FileType vimfiler nunmap <buffer> <C-l>
autocmd FileType vimfiler nmap <buffer> r   <Plug>(vimfiler_redraw_screen)

nmap <silent><buffer><expr> <Cr> vimfiler#smart_cursor_map(
      \ "\<Plug>(vimfiler_expand_tree)",
      \ "\<Plug>(vimfiler_edit_file)")

nnoremap <C-e> :VimFilerExplorer -parent -toggle -status -split -winwidth=30 -no-quit<CR>

" session management
let g:session_directory = "~/.config/nvim/session"
let g:session_autoload = "yes"
let g:session_autosave = "yes"
let g:session_command_aliases = 1

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
noremap <Leader>h :<C-u>split<CR>
noremap <Leader>v :<C-u>vsplit<CR>

" session management
nnoremap <leader>so :OpenSession<Space>
nnoremap <leader>ss :SaveSession<Space>
nnoremap <leader>sd :DeleteSession<CR>
nnoremap <leader>sc :CloseSession<CR>

"" Tabs
nnoremap <Tab> gt
nnoremap <S-Tab> gT
nnoremap <silent> <S-t> :tabnew<CR>

" Tagbar
map <Leader>tt :TagbarToggle<CR>
"let g:tagbar_autofocus = 1

"" Copy/Paste/Cut
set clipboard=unnamed

"" Buffer nav
noremap <leader>z :bp<CR>
noremap <leader>q :bp<CR>
noremap <leader>x :bn<CR>
noremap <leader>w :bn<CR>

"" Close buffer
noremap <leader>c :bd<CR>

"" Clean search (highlight)
nnoremap <silent> <leader><space> :noh<cr>

"" Switching windows
nmap <BS> <C-W>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
noremap <C-h> <C-w>h

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

nmap <silent> <C-z> <Plug>(ale_previous_wrap)
nmap <silent> <C-x> <Plug>(ale_next_wrap)

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

"*****************************************************************************
"" Custom command maps
"*****************************************************************************

" Change Working Directory to that of the current file
cmap cd. lcd %:p:h

" For when you forget to sudo.. Really Write the file.
cmap w!! w !sudo tee % >/dev/null

"*****************************************************************************
"" Auto complete configuration
"*****************************************************************************
let g:deoplete#enable_at_startup = 1
let g:deoplete#complete_method="omnifunc"

let g:deoplete#omni_patterns = {}

augroup omnifuncs
  autocmd!
  autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
 autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
  autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
  autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
augroup end

autocmd FileType javascript let g:SuperTabDefaultCompletionType = "<c-x><c-o>"
let g:UltiSnipsExpandTrigger="<C-j>"
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

" close the preview window when you're not using it
let g:SuperTabClosePreviewOnPopupClose = 1

"*****************************************************************************
"" Self Customise
"*****************************************************************************
let g:WebDevIconsOS = 'Darwin'

""Hard Mode
nnoremap <up>    <nop>
nnoremap <down>  <nop>
nnoremap <left>  <nop>
nnoremap <right> <nop>
inoremap <up>    <nop>
inoremap <down>  <nop>
inoremap <left>  <nop>
inoremap <right> <nop>
