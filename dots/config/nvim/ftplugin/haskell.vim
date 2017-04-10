" Disable haskell-vim omnifunc
let g:haskellmode_completion_ghc = 0
" Show detailed information (type) of symbols.
let g:necoghc_enable_detailed_browse = 1
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

