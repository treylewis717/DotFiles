set nocompatible              " be iMproved, required
filetype off                  " required

" Vundle
    set rtp+=~/.vim/bundle/Vundle.vim
    call vundle#begin()


    " Vundle itself
    Plugin 'gmarik/Vundle.vim'

    "Lightline
    Plugin 'itchyny/lightline.vim'

    "Markdown Preview
    Plugin 'suan/vim-instant-markdown', {'rtp': 'after'}

    "CSS Color Previews
    Plugin 'ap/vim-css-color'

    "Emojis
    Plugin 'junegunn/vim-emoji'

    call vundle#end()
    filetype plugin indent on
    " To ignore plugin indent changes, instead use:
    "filetype plugin on
    "
    " Brief help
    " :PluginList       - lists configured plugins
    " :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
    " :PluginSearch foo - searches for foo; append `!` to refresh local cache
    " :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
    "
    " see :h vundle for more details or wiki for FAQ
    " Put your non-Plugin stuff after this line


" Status Line
    let g:lightline = {
          \ 'colorscheme': 'darcula',
          \ }
    " Always show statusline
    set laststatus=2

    " Prevents non-normal modes showing in powerline and below powerline
    set noshowmode
