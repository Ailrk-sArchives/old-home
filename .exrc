command! -nargs=0 APPKill :!python ./scripts/kill_server.py
command! -nargs=0 APPTrack :silent !python ./scripts/track.py -file % <bar> :NERDTreeRefreshRoot
command! -nargs=0 APPUntrack :silent !python ./scripts/track.py -file % -untrack <bar> :NERDTreeRefreshRoot
