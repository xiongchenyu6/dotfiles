scriptencoding utf-8
source ~/.config/nvim/plugins.vim

"*****************************************************************************
"" Basic Setup
"*****************************************************************************"

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

"*****************************************************************************
"" Visual Settings
"*****************************************************************************
set number
set spell
"set foldenable
set foldmethod=syntax
set foldlevelstart=99

set tags=./.vimtags,vimtags;

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

let g:unite_prompt = "➤ "
let g:unite_winheight = 20
let g:unite_split_rule = 'botright'
let g:unite_enable_ignore_case = 1
let g:unite_enable_smart_case = 1
let g:unite_enable_start_insert = 1

let g:unite_source_file_mru_limit = 200
let g:unite_source_history_yank_enable = 1
let g:unite_source_rec_max_cache_files=5000

nnoremap <Leader>/  :Unite grep:.<cr>
nnoremap <Leader>f  :Unite file_rec/async<CR>
nnoremap <Leader>y  :Unite history/yank<CR>
"nnoremap <Leader>s  :Unite -quick-match buffer<CR>
"nnoremap <Leader>n  :Unite -buffer-name=New -profile-name=files file/new<CR>

autocmd FileType unite call s:unite_my_settings()
function! s:unite_my_settings()
  imap <silent><buffer> <C-k> <C-p>
  imap <silent><buffer> <C-j> <C-n>
  imap <silent><buffer> <C-d> <CR>
  call unite#filters#matcher_default#use(['matcher_fuzzy'])
  call unite#filters#sorter_default#use(['sorter_rank'])
  call unite#custom#source('file_rec,file_rec/async', 'ignore_pattern', '(\.meta$|\.tmp)')
endfunction

if executable('ag')
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_rec_async_command = 'ag --follow --nocolor --nogroup -g ""'
  "let g:unite_source_grep_default_opts='--nocolor --nogroup --column'
  let g:unite_source_grep_default_opts = '--line-numbers --nocolor --nogroup --hidden --ignore ' .
        \  '''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'''
  let g:unite_source_grep_recursive_opt = ''
endif

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
autocmd FileType vimfiler nunmap <buffer> <S-m>
autocmd FileType vimfiler nmap <buffer> r   <Plug>(vimfiler_redraw_screen)
autocmd FileType vimfiler nmap <buffer> u   <Plug>(vimfiler_switch_to_parent_directory)
autocmd FileType vimfiler nmap <buffer> <Leader>n           <Plug>(vimfiler_new_file)
autocmd FileType vimfiler nmap <buffer> <silent><Leader>r   <Plug>(vimfiler_rename_file)
autocmd FileType vimfiler nmap <buffer> <silent><Leader>m   <Plug>(vimfiler_move_file)
autocmd FileType vimfiler nmap <buffer> <S-m-k> <Plug>(vimfiler_make_directory)

nmap <silent><buffer><expr> <Cr> vimfiler#smart_cursor_map(
      \ "\<Plug>(vimfiler_expand_tree)",
      \ "\<Plug>(vimfiler_edit_file)")

nnoremap <C-o> :VimFilerExplorer -parent -toggle -status -split -simple -winwidth=30 -no-quit<CR>
