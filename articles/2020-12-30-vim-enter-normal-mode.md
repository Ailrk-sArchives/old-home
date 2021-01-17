-- tag Vim casual
-- title How to enter Vim normal mode?
-- time 2021-01-16
-- source https://vim.fandom.com/wiki/Avoid_the_escape_key
          https://en.wikipedia.org/wiki/ADM-3A
          https://www.howtogeek.com/683823/why-does-the-caps-lock-key-exist-and-why-was-it-created/
;;
### Different ways to enter Vim normal mode

<br/>

Vim is famous for it's modal editing. You insert text in the insert mode, and do almost everything else in the normal mode. The switching happens so frequently that even the slightest overhead makes it feels super cumbersome. I believe most Vim users sooner or later will throw the same question: why `Esc`? It seems the least suitable key for such an important job. It's far away, it's small, and it stretches the left middle finger unnecessarily. It turns out there is a historical reason: Vim was developed with a keyboard layout that `Esc` was at the position of `Tab` today. It doesn't seem to be a bad decision back then. However, since most people are not gonna use the old keyboard layout, it doesn't make sense to stick with `Esc` anymore. There are several well known alternatives like `C-c` and `Capslock`. Alhough which one to use is largely up to the taste, there are some quirks that you won't notice until you start to use them. I've tried 4 different alternatives, and here is what I find out.

<figure>
    <img style="display: block; margin: auto;" src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/a0/KB_Terminal_ADM3A.svg/640px-KB_Terminal_ADM3A.svg.png">
    <figcaption align="center"> <i>ADM-3A, the layout that Vim was designed for. More history <a href="https://www.reddit.com/r/vim/comments/dvvbbi/how_many_people_actually_use_the_escape_key/">here</a></i>
    </figcaption>
</figure>

##### Map to CapsLock
The first one I tried is to map `CapsLock` to `Esc`. At the first glance it might be the best solution. It's only one key, you don't need to press a key chord; the button is big and obvious; because `CapsLock` is roughly at the same position of the old `Esc`, one can even argue that this binding inherits the true Vim spirit. It all sounds nice, but once you've been using it for a while, you will find it's really hard to get used to. The biggest problem is that `CapsLock` is very close to `A` and `Tab`, which are commonly used for entering insert mode and triggering autocompletion. These keys are used so frequently yet so close, it's very easy to make a typo. It gets worse, the left most keyboard is probably governed by your left hand ring finger or pinky, which are both weak and hard to control. Imagine you want to enter normal mode, but typed `Tab` instead: all the sudden your cursor jumps a tab away, and now you need to go back to retry again. This can be really frustrating.

The advantage of doing so, of course, is that you can finally put `CapsLock` in some good use. To enable the map, you need to switch `Esc` and `CapsLock` in the x window level directly. (Assume on Linux). It's because `CapsLock` sends the interrupt to the OS directly before Vim can do anything with it. For people don't want to search on Stackoverflow, here is how you set it up if you use x windows:

```
$ xmodmap -e 'clear Lock' -e 'keycode 0x42' = Escape'
```

##### Ctrl-C
`C-c` doesn't have most of problems `CapsLock` have, and it just feel so natural to exit insert mode with `Ctrl-C`, like how you do it in the shell. Except... it's not really an `Esc` replacement. Instead, just like in a shell, it acts like sending a `SIGINT` and stops what's happening immediately. If you have abbreviations, `Esc` will expand them first before entering normal mode, but `C-c` will just leave what you've typed and switch mode directly.

For me, there is a bigger issue: most editors' Vim mode doesn't support `C-c`, so the muscle memory doesn't transfer. Sometimes you are forced to use certain tools. For instance android studio or emacs. You can always fall back to other alternatives, but it will be nice to use Vim the same way everywhere.

##### Ctrl-[
One of the nice thing about `Ctrl-[` is that it behaves exactly the same as `Esc`, which makes it an ideal alternative. The key chord is not too hard to press, although it's still harder than `C-c` and `CapsLock`, but according to my experience, once you're used to it, the accuracy is pretty reliable. `Ctrl-[` is implemented on most Vim plugins, so you can use them everywhere. If you really ask for a problem, I guess it might be that it's too close to `p`, which makes you accidentally paste bunch of text. Other than that, It might be the best alternative for `Esc`.

##### Alt + motion keys
I discovered this shortcut quite recently, but it terms out it's immediately useful. The idea is that when you are in the editing mode, you can press `Alt` plus another motion key, `h, i, j, k` so to speak. This will bring you to the normal mode, and perform the motion directly right after. The advantage is that it's super easy to type, even when you are in insert mode, you can pretend you are already in the normal mode, and all the sudden the mode switching becomes seamless. An exmaple: I have a mapping that completes the right brace if I type a left brace, then place the cursor in the middle. It's nice if I need to fill the brace, but for typing things like function without parameter, you want to move the cursor after the right brace. Without `Alt+motion`, you have two options: you can either press some `Esc` equivalence, then press `l` twice; or you can use `Ctrl-O`, which temporarily brings you to the normal mode for one command, and then press `A` to move the cursor one after. These two methods both require you to press a intermediate key for transition (`Esc` and `Ctrl-O`). But if you use `Alt+motion`, you can combine mode switching and motion into on key chord. It seems only save on key press, but because mode switching is quite common, it can save quite a bit of typing.

It's worth noting that `ALt+motion` can't replace `Esc` for all situations. For example, you can close a temporary window with `Esc`, but `Alt+motion` will do nothing at all.

##### Conclusion
I have summarized these four alternatives into a table.
<table>
    <tr style="color: LightCoral;">
        <td> Key/Key chord </td>
        <td>Same behavior as ESC?</td>
        <td>Easy to type</td>
        <td>Vim mode support? </td>
    </tr>
    <tr>
        <td>CapsLock</td>
        <td>yes</td>
        <td>3rd</td>
        <td>yes (need to map in the OS level)</td>
    </tr>
    <tr>
        <td>C-c</td>
        <td>no</td>
        <td>2ed</td>
        <td>no</td>
    </tr>
    <tr>
        <td>C-[</td>
        <td>yes</td>
        <td>4th</td>
        <td>yes</td>
    </tr>
    <tr>
        <td>Alt-motion</td>
        <td>no</td>
        <td>1st</td>
        <td>partially(emacs doesn't support)</td>
    </tr>
</table>

Now I primarily use `ALt+motion` for switching mode. In the rare case of closing quickfix windows, I will choose `C-[`. Although `C-[` is probably not the easiest key chord to press, it's very transferable. To be honest, if one sticks with `alt` for most of them time, occasional `Esc` might also be tolerable. Again, it's only my opinion, and the actual situation might vary for different people. The best part of using Vim is that you can constantly find new ways to micro optimize your work flow. Even if you are quite happy with your current configuration, there will always be more efficient way to do things. Keep hacking it up!
