-- tag note haskell xmonad zipper data-structure
-- title A little xmonad case study
-- date 2020-08-14
-- source https://en.wikibooks.org/wiki/Haskell/Zippers
          https://github.com/xmonad/xmonad
          https://wiki.haskell.org/Xmonad/Guided_tour_of_the_xmonad_source
;;
# A little xmonas case study
Everybody seems to like xmonad so much, but I always feel like it's because other than xmonad there are not many popular haskell applications to talk about. Even for xmonad, tiling window manager is already kind of a niche program itself.

The value of xmonad is it manage state in a purely functional way. There is probably nothing more stateful than window moving on the screen, and Xmonad proved haskell can do it very well.

## Module Structure
```
                Main
                 ↓
 Operartions ←-XMonad
  +    |         |
  |    |   +-----+---------+-----+
  |    ↓   ↓     ↓         ↓     |
  |   Layout  Config  ManageHook |
  |       ↓     ↓         ↓      |
  |       +-----+---------+      |
  |             ↓                |
  +----------→ Core ←------------+
                ↓
             StackSet
```

## Window manager
Xmonad is a tiling window manger. A tiling window manager can be modeled as a set of virtual workspaces. On each workspaces there is a stack of windows. On each workspaces there will be one winodow that currently on focus. A window is on current workspace and currently on focus will be the one that takes user input.

