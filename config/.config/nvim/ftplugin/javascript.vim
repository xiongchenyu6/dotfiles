" Use deoplete.
let g:tern_request_timeout = 1
let g:tern_show_signature_in_pum = '0'  " This do disable full signature type on autocomplete

"Add extra filetypes
let g:tern#filetypes = [
                \ 'jsx',
                \ 'javascript.jsx',
                \ 'vue',
                \ ]

let g:deoplete#omni#functions = get(g:, 'deoplete#omni#functions', {})

let g:deoplete#omni#functions.javascript = [
		  \ 'tern#Complete',
		  \ 'jspc#omni'
      \]

