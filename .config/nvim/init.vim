set nocompatible              " be iMproved, required
filetype off                  " required
set shell=/bin/bash
filetype plugin indent on
lua require('plugins')

" General Settings
    set expandtab    " Spaces instead of tabs
    set smarttab     " SmartTab
    set shiftwidth=4 " One tab == 4 spaces
    set tabstop=4    " One tab == 4 spaces
    set t_Co=256     " Set if term supports 256 bit color
    let g:python_highlight_all = 1
    syntax enable

" Keybindings
    " Open terminal inside vim
    map <Leader>tt :vnew term://fish<CR>

" Vimwiki
    let g:vimwiki_list = [{'path': '~/vimwiki/',
                          \ 'syntax': 'markdown', 'ext': '.md'}]
