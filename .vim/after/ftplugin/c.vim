if !filereadable(getcwd()."/Makefile")
    setlocal makeprg=gcc\ -Wall\ -Wextra\ -o\ %<\ %
endif

setlocal efm=%f:%l:%c:\ %trror:\ %m
setlocal efm+=%f:%l:%c:\ %tarning:\ %m
setlocal efm+=%.%#:\ %f:%l:\ %.%#:\ Assertion\ `%m'\ failed.
setlocal efm+=Assertion\ failed:\ (%m)\\,\ function\ %.%#\\,\ file\ %f\\,\ line\ %l.
setlocal efm+=%-G%.%#

nnoremap <leader>r :w \| make \| call RunFile()<cr>
nnoremap <leader>t :w \| make test<cr>

function! RunFile()
    if !filereadable(expand("%:p:h")."/Makefile")
        !./%:r 2>&1
    else
        :make run
    end
endfunction
