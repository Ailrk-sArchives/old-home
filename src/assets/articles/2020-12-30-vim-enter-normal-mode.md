-- tag Vim casual
-- title How to enter Vim normal mode?
-- time 2020-12-30
-- source https://vim.fandom.com/wiki/Avoid_the_escape_key
          https://en.wikipedia.org/wiki/ADM-3A
          https://www.howtogeek.com/683823/why-does-the-caps-lock-key-exist-and-why-was-it-created/
;;
### Different ways to enter Vim normal mode

Vim is famous for it's modal editing. You insert text in the insert mode, and do almost everything else in the normal mode. The switching happens so frequently that even the slightest overhead makes it feels super cumbersome. I believe most Vim users sooner or later will throw the same question: why `Esc`? It seems the least suitable key for such an important job. It's far away, it's small, and it stretches the left middle finger unnecessarily. It turns out there is a historical reason: Vim was developed with a keyboard layout that `Esc` was at the position of `Tab` today. It doesn't seem to be a bad decision back then. However, since most people are not gonna use the old keyboard layout, it doesn't make sense to stick with `Esc` anymore. There are several well known alternatives like `C-c` and `Capslock`. Thought which one to use is largely up to the taste, there are some quirks that you won't find out until you try them out. I've tried 4 different alternatives, and here is what I find out.

<figure>
    <img style="display: block; margin: auto;" src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/a0/KB_Terminal_ADM3A.svg/640px-KB_Terminal_ADM3A.svg.png">
    <figcaption align="center"> <i>ADM-3A, the layout that Vim was designed for. More history <a href="https://www.reddit.com/r/vim/comments/dvvbbi/how_many_people_actually_use_the_escape_key/">here</a></i>
    </figcaption>
</figure>

##### Map to CapsLock
First one I tried is to map `CapsLock` to `Esc`. At the first glance it might be the best solution. It's only one key, you don't need to press a key chord; the button is big and obvious; because `CapsLock` is roughly at the same position of the old `Esc`, one can even argue that this binding inherits the true Vim spirit. It all sounds nice, but once you've been using it for a while, you will find it's really hard to get used to. The biggest problem is that `CapsLock` is very close to `A` and `Tab`, which are commonly used for entering insert mode and triggering autocompletion. These keys are used so frequently, and they are so close, it's very easy to make a typo. It gets worse, the left most keyboard is probably governed by your left hand ring finger or pinky, which are both weak and hard to control. Imagine you want to enter normal mode, but typed `Tab` instead. All the sudden your cursor jumps a tab away, and now you need to go back to retry again. This can be really frustrating.

The advantage of doing so, of course, is that you can finally put `CapsLock` in some good use. To enable the map, you need to switch `Esc` and `CapsLock` in the x window level directly. (Assume on Linux). It's because `CapsLock` sends the interrupt to the OS directly before Vim can do anything with it. For people don't want to search on Stackoverflow:

```
$ xmodmap -e 'clear Lock' -e 'keycode 0x42' = Escape'
```

##### Ctrl-C
`C-c` doesn't have most of problems `CapsLock` has, and it just feel so natural to exit insert mode with `Ctrl-C`, like how you do it in the shell. Except... it's not really an `Esc` replacement. Instead, just like in a shell, it's act like sending a `SIGINT` and stop what's happening immediately. If you have abbreviation, `Esc` will expand that then enter normal mode, but `C-c` will just leave what you've typed and switch mode directly.

For me, there is a bigger issue: most editors' Vim mode doesn't support `C-c`, so the muscle memory doesn't transfer. Sometimes you are forced to use certain tools. For instance android studio or emacs. You can always fall back to other alternatives, but it will be nice to use Vim the same way everywhere.

##### Ctrl-[
One of the nice thing about `Ctrl-[` is that it behaves exactly the same as `Esc`, which makes it an ideal alternative. The key chord is not too hard to press, although it's still harder than `C-c` and `CapsLock`, but according to my experience, once you're used to it, the accuracy is pretty reliable. `Ctrl-[` is implemented on most Vim plugins, so you can use them everywhere. If you really want to find a problem for it, I guess it might be that it's too close to `p`, which makes you accidentally paste bunch of text unnecessarily. Other than that, I feel it's the directly alternative for `Esc`.

##### Alt + motion keys
I discovered this shortcut quite recently, but it terms out it's immediately useful. The idea is that when you are in the editing mode, you can press `Alt` plus another motion key, `h, i, j, k` so to speak. This will bring you to the normal mode, and perform the motion directly right after. Advantage is that it's super easy to type, even when you are in insert mode, you can pretend you are already in the normal mode, and all the sudden the connection becomes seamless. I have a mapping to complete the left brace with a right brace, and place the bracket in the middle. It's nice that after I press the left brace I can fill the content, but for things like function without parameter, what you reall want is move the cursor after the right brace. Without `Alt+motion`, you have two options: you either need to press some `Esc` equivalence, then press `l` twice; or you can use `Ctrl-O`, which temporarily brings your to the normal mode for one command, and then press `A` to move the cursor one after. `Alt+motion` allows you switch without any overhead, which I think is really valuable.

It's worth noting that `ALt+motion` can't replace `Esc` for all cases. For example, you can close a temporary buffer with `Esc`, but `Alt+motion` will do nothing at all. But for most of the cases it is a good enough alternative that it's tolerable to use a `Esc` only for special cases.

##### Conclusion

<table>
    <tr>
        <td> Key/Key chord </td>
        <td>Has the same behavior as ESC? (y/n)</td>
        <td>Easy to type (rank)</td>
        <td>Support by most editor's Vim mode? (y/n)</td>
    </tr>
     <tr>
        <td>CapsLock</td>
        <td>yes</td>
        <td>2ed</td>
        <td>yes (need to map in the OS level)</td>
    </tr>
      <tr>
        <td>C-[</td>
        <td></td>
        <td></td>
        <td></td>
    </tr>
       <tr>
        <td>CapsLock</td>
        <td></td>
        <td></td>
        <td></td>
    </tr>
</table>

Now I use `ALt+motion` for most of the time. When I really `Esc`, I will choose `C-[`. This way you get the best ergonomics without the need to worry about transferability. Of course this is only my personal preference, it really up to your experience. The best part of using Vim is that you can constantly find some way to do micro optimize your work flow. Each improvement might seem trivial if you look at them separately, once you look back it feels that you're able to do so much more.
