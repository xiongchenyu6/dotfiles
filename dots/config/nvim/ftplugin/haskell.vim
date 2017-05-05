" Disable haskell-vim omnifunc
let g:haskellmode_completion_ghc = 0
" Show detailed information (type) of symbols.
let g:necoghc_enable_detailed_browse = 1
autocmd FileType haskell setlocal omnifunc=intero#omnifunc

"GHC-MOD
map <silent> ,y :GhcModTypeInsert<CR>
map <silent> ,s :GhcModSplitFunCase<CR>
map <silent> ,t :GhcModType<CR>
map <silent> ,c :GhcModTypeClear<CR>
map <silent> ,cc :GhcModCheckAndLintAsync<CR>

let g:easytags_languages = {
\   'haskell': {
\       'cmd': 'lushtags',
\       'args': [],
\       'fileoutput_opt': '-f',
\       'stdout_opt': '-f-',
\       'recurse_flag': '-R'
\   }
\}
