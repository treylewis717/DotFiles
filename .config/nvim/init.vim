set nocompatible              " be iMproved, required
filetype off                  " required
set shell=/bin/bash

" vim-plug
    call plug#begin('~/.vim/plugged')

    " Lightline
    Plug 'itchyny/lightline.vim'

    " Markdown Preview
    Plug 'suan/vim-instant-markdown', {'rtp': 'after'}

    " GoCode
    Plug 'nsf/gocode', {'rtp': 'nvim/'}

    " CSS Color Previews
    Plug 'ap/vim-css-color'

    " Emojis
    Plug 'junegunn/vim-emoji'

    " Rustic Syntax Support
    Plug 'rust-lang/rust.vim'

    " Vifm
    Plug 'vifm/vifm.vim'

    " Python Syntax Support
    Plug 'vim-python/python-syntax'

    " VimWiki
    Plug 'vimwiki/vimwiki'

    " Magit for vim
    Plug 'jreybert/vimagit'

    call plug#end()
    filetype plugin indent on

" General Settings
    set expandtab    " Spaces instead of tabs
    set smarttab     " SmartTab
    set shiftwidth=4 " One tab == 4 spaces
    set tabstop=4    " One tab == 4 spaces
    set t_Co=256     " Set if term supports 256 bit color
    let g:python_highlight_all = 1
    syntax enable

" lightline
    let g:lightline = {
          \ 'colorscheme': 'darcula',
          \ }
    " Always show statusline
    set laststatus=2

    " Prevents non-normal modes showing in powerline and below powerline
    set noshowmode

" Keybindings
    " Open terminal inside vim
    map <Leader>tt :vnew term://fish<CR>

" Vimwiki
    let g:vimwiki_list = [{'path': '~/vimwiki/',
                          \ 'syntax': 'markdown', 'ext': '.md'}]
