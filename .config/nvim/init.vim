if &compatible
    set nocompatible
endif
"  Dein {{{
set runtimepath+=~/.vim/bundle/repos/github.com/Shougo/dein.vim
if dein#load_state(expand('~/.vim/bundle'))
    call dein#begin('~/.vim/bundle')
    """ Let Dein manage itself
    call dein#add('Shougo/dein.vim')

    """ Color Schemes
    call dein#add('w0ng/vim-hybrid')

    " Deoplete (Code Completion) {{{
    call dein#add('Shougo/deoplete.nvim', {'on_event': 'InsertEnter'})
    "call dein#add('zchee/deoplete-clang')                 " C/C++
    "call dein#add('zchee/deoplete-go',
                "\ {'build': 'make', 'on_ft': 'go'})       " Go
    call dein#add('sebastianmarkow/deoplete-rust',
                \ {'on_ft': 'rust'})                      " Rust
    call dein#add('Shougo/neco-vim',
                \ {'on_ft': 'vim'})                       " Vim
    call dein#add('zchee/deoplete-jedi',
                \ {'on_ft': 'python'})                    " Python
    call dein#add('eagletmt/neco-ghc',
                \ {'on_ft': 'haskell'})                   " Haskell
    " }}}

    " Airline {{{
    call dein#add('bling/vim-airline')
    call dein#add('vim-airline/vim-airline-themes')
    " }}}

    " Misc Utilities {{{
    call dein#add('amiorin/vim-fenced-code-blocks',
                \ {'on_ft': 'markdown'})
    " call dein#add('vim-utils/vim-man')
    call dein#add('dhruvasagar/vim-markify',
                \ {'on_event': 'QuickFixCmdPost'})
    " call dein#add('tpope/vim-speeddating')
    " call dein#add('kien/ctrlp.vim')
    " call dein#add('tacahiroy/ctrlp-funky')
    " call dein#add('tpope/vim-fugitive')
    " call dein#add('tpope/vim-rhubarb')
    call dein#add('airblade/vim-gitgutter',
                \ {'on_event': 'CursorHold'})
    call dein#add('tpope/vim-surround')
    call dein#add('tpope/vim-repeat')
    " call dein#add('godlygeek/tabular')
    " call dein#add('scrooloose/nerdcommenter')
    " call dein#add('ap/vim-css-color')
    " }}}

    " Text Objects {{{
    " i,w is camelCase word
    call dein#add('bkad/CamelCaseMotion')

    " aa and ia are arguments in a function call
    call dein#add('vim-scripts/argtextobj.vim')

    call dein#add('kana/vim-textobj-user')

    " ar and ir are Ruby blocks
    call dein#add('nelstrom/vim-textobj-rubyblock',
                \ {'on_ft': 'ruby'})
    " }}}

    " Languages {{{
    "" Markup Languages {{{{
    call dein#add('digitaltoad/vim-jade',
                \ {'on_ft': 'jade'})          " Jade
    call dein#add('slim-template/vim-slim',
                \ {'on_ft': 'slim'})          " Slim
    call dein#add('groenewege/vim-less',
                \ {'on_ft': 'less'})          " Less
    "" }}}}
    "" Configuration {{{{
    call dein#add('stephpy/vim-yaml',
                \ {'on_ft': 'yaml'})          " YAML
    call dein#add('tpope/vim-haml',
                \ {'on_ft': 'haml'})          " HAML
    "" }}}}
    "" Javascript {{{{
    call dein#add('jelera/vim-javascript-syntax',
                \ {'on_ft': 'javascript'})
    call dein#add('pangloss/vim-javascript',
                \ {'on_ft': 'javascript'})
    call dein#add('crusoexia/vim-javascript-lib',
                \ {'on_ft': 'javascript'})
    "" }}}}
    call dein#add('rust-lang/rust.vim',
                \ {'on_ft': 'rust'})          " Rust
    call dein#add('rhysd/vim-crystal',
                \ {'on_ft': 'crystal'})       " Crystal
    call dein#add('LnL7/vim-nix',
                \ {'on_ft': 'nix'})           " Nix
    call dein#add('jceb/vim-orgmode',
                \ {'on_ft': 'org'})           " Orgmode
    call dein#add('kchmck/vim-coffee-script',
                \ {'on_ft': 'coffee'})        " Coffee Script
    call dein#add('vim-ruby/vim-ruby',
                \ {'on_ft': 'ruby'})          " Ruby
    call dein#add('tpope/vim-rails',
                \ {'on_ft': 'ruby'})          " Rails
    call dein#add('dag/vim2hs',
                \ {'on_ft': 'haskell'})       " Haskell
    call dein#add('sophacles/vim-processing',
                \ {'on_ft': 'processing'})    " Processing
    call dein#add('rudrab/vimf90',
                \ {'on_ft': 'fortran'})       " Fortran
    call dein#add('tkztmk/vim-vala',
                \ {'on_ft': 'vala'})          " Vala
    " }}}

    call dein#end()
    call dein#save_state()
endif
filetype plugin indent on    " required
syntax enable
" }}}

" Basics {{{
let g:nixprofile = expand('~/.nix-profile')
" allow unsaved background buffers and remember marks/undo for them
set hidden
" remember more commands and search history
set history=10000
set cmdheight=1
set switchbuf=useopen
" use emacs-style tab completion when selecting files, etc
set wildmode=longest,list
" make tab completion for files/buffers act like bash
set wildmenu
" use system clipboard
set clipboard=unnamedplus
" set leader key
let mapleader = " "
" Normally, Vim messes with iskeyword when you open a shell file. This can
" leak out, polluting other file types even after a 'set ft=' change. This
" variable prevents the iskeyword change so it can't hurt anyone.
let g:sh_noisk=1
" Insert only one space when joining lines that contain sentence-terminating
" punctuation like `.`.
set nojoinspaces
" If a file is changed outside of vim, automatically reload it without asking
set autoread
" }}}

" Last Position Jump {{{
autocmd BufReadPost * if @% !~# '\.git[\/\\]COMMIT_EDITMSG$' && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
" }}}

" Colors {{{
set number
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
set t_Co=256
set background=dark
colorscheme hybrid
" }}}

" Spaces and Tabs {{{
set tabstop=4 " number of visual spaces per TAB
set softtabstop=4 " number of spaces in tab when editing
set shiftwidth=4
set expandtab " makes tabs spaces
set modelines=1
set backspace=indent,eol,start
set autoindent
set copyindent
set smartindent
" }}}

" Invisibles {{{
set list
set listchars=tab:▸\ ,trail:·
"}}}

" UI {{{
set showcmd
set cursorline
set wildmenu
set showmatch
set showtabline=2
set winwidth=79
" keep more context when scrolling off the end of a buffer
set scrolloff=3
" }}}

" Custom Mappings {{{
" make ; the command key
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;
" Open and source .vimrc file
nnoremap <leader>fed :edit $MYVIMRC<cr>
nnoremap <leader>feR :source $MYVIMRC<cr>
" use visual line up and down
nnoremap j gj
nnoremap k gk
" make fd switch to command mode
inoremap fd <esc>
vnoremap fd <esc>
"Move splits more easily
nnoremap <leader>wj <c-w>j
nnoremap <leader>wk <c-w>k
nnoremap <leader>wh <c-w>h
nnoremap <leader>wl <c-w>l
" Spacemacs-like
nnoremap <leader>fs :w<cr>
" }}}

" Git Gutter {{{
let g:gitgutter_enabled = 0
nnoremap <leader>gg :GitGutterToggle<cr>
nnoremap <leader>gr :GitGutterRevertHunk<cr>
nnoremap <leader>gn :GitGutterNextHunk<cr>
nnoremap <leader>gp :GitGutterPrevHunk<cr>
" }}}

" Search {{{
set incsearch
set hlsearch
" make searches case-sensitive only if they contain upper-case characters
set ignorecase smartcase
" }}}

" Folding {{{
set foldenable
set foldlevelstart=10
set foldnestmax=10
set foldmethod=indent
" }}}

" Airline {{{
let g:airline_powerline_fonts = 0
let g:airline_theme = 'kalisi'
set laststatus=2 " Always display the statusline in all windows
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)
set fillchars+=stl:\ ,stlnc:\
set termencoding=utf-8
" }}}

" Markify {{{
let g:markify_error_text='✗✗'
let g:markify_warning_text='❢❢'
" }}}

" Deoplete {{{
" Enable on startup
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_refresh_always = 1
" Use tab key
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
inoremap <expr><s-tab> pumvisible() ? "\<c-p>" : "\<s-tab>"
"if has('mac')
"    let g:libclang_path = g:nixprofile . '/lib/libclang.dylib'
"else
"    let g:libclang_path = g:nixprofile . '/lib/libclang.so'
"end
"" C/C++ {{{{
"let g:deoplete#sources#clang#libclang_path = g:libclang_path
"let g:deoplete#sources#clang#clang_header = g:nixprofile . '/lib/clang'
"" }}}}
"" Go {{{{
let g:deoplete#sources#go#gocode_binary = g:nixprofile . '/bin/gocode'
let g:deoplete#sources#go#sort_class = ['package', 'func', 'type', 'var', 'const']
let g:deoplete#sources#go#pointer = 1
let g:deoplete#sources#go#use_cache = 1
let g:deoplete#sources#go#json_directory = '~/.cache/deoplete/go/$GOOS_$GOARCH'
let g:deoplete#sources#go#cgo = 1
"let g:deoplete#sources#go#cgo#libclang_path = g:libclang_path
"" }}}}
"" Rust {{{{
let g:deoplete#sources#rust#racer_binary = g:nixprofile . '/bin/racer'
let g:deoplete#sources#rust#rust_source_path = './src'
"" }}}}
"" Haskell {{{{
let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
"" }}}}
" }}}

" Git Commit {{{
autocmd FileType gitcommit setlocal spell
" }}}

" Persistent Undo {{{
set undofile
set undodir=~/.cache/nvim/undo
" }}}

" vim:foldmethod=marker:foldlevel=0
