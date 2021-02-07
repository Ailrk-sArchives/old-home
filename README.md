### Personal blog

This app serves as both personal blog and note manager. Public articles are put in `articles/` folder, and notes are in `notes` foler.

### Vim commands
There is a `.exrc` file under the root directory provides some vim commands to make editing easier.
```
# To kill a node process listening on <port>. Non node process will not be
# affected.
:APPKill <port>

# Track the current file change. Once called, current file will be moved to
# src/assets/abuf, and a symlink to the abuf file will replace the original
# file.
:APPTrack

# Undo APPTrack
:APPUntrack

# Refresh
:APPRefresh
```

#### Todo

* [x] Chronological order
* [x] Clickable tag
* [x] tag search
* [x] tag list
* [x] sidebar
* [x] styling
