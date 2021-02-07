command! -nargs=0 APPKill :!python ./scripts/kill_server.py
command! -nargs=0 APPTrack :call Track()
command! -nargs=0 APPUntrack :call Untrack()
command! -nargs=0 APPRefresh :silent !touch -r % ./src/state/markdowns.ts
function! Track()
    :silent !python ./scripts/track.py -file %
    :NERDTreeRefreshRoot
endfunction

function! Untrack()
    :silent !python ./scripts/track.py -file % -untrack
    :NERDTreeRefreshRoot
endfunction
