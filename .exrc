command! -nargs=0 APPKill :!python ./scripts/kill_server.py
command! -nargs=0 APPStart :silent !npm run start 2>/dev/null &

