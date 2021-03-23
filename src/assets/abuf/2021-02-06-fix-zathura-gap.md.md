-- tag zathura c causual oop
-- title A Random day - Fixing zathura gap
-- time 2021-02-04
-- source https://github.com/ailrk/zathura
          https://pwmt.org/projects/zathura/
          https://folk.ntnu.no/haugwarb/Programming/OHaskell/o_haskell_tutorial.html#sect4
          https://news.ycombinator.com/item?id=4784116
;;
### A Random day: Fixing zathura gap

<br/>

###### Off topic warning: this article is a record for studying some zathura code base, but it also contains discussion of other irrelevanat topics like oop and GUI, GTK and c idioms etc. I need to learn how to write properly...

<br/>

I always wanted a pdf reader with good vim navigation support. I tried mupdf for a while, but it was too primitive for any serious task, it's intended to be used as a pdf renderer library after all. So later I switch to zathura, a highly customizable vim like pdf reader. It's as simple as mupdf, you have some nice vim key bindings that allows you to move around in the document. Zathura provides some useful facillties like bookmarks and table of contents, and you can almost customize everything with it's built-in command interface.

I was very satisfied with it, but soon I hit a problem that all the sudden makes zathura usable. Ideally, all pages in a pdf should be of the same size. But for some pdf there might be a page that is significantly larger then others. For example, some scanned pdf will have a huge cover page. Normally a pdf reader will arrange pages in a way that the gap between pages are small, so there can be more room for showing the content. It's not the case for zathura though. If there is a page much larger than others, it's height and width will be used to render all other pages, so you ends up with something like this:


<figure>
    <img style="display: block; margin: auto;" height="400"src="https://user-images.githubusercontent.com/12954634/106982762-877ea780-6719-11eb-808c-f1cc5c962272.png">
    <figcaption align="center"> <i>Huge gap</i>
    </figcaption>
</figure>

But what you really want is to just ignore the outlier, and try to make the gap between pages as small as possible. Something like this:

<figure>
    <img style="display: block; margin: auto;" height="400"src="https://user-images.githubusercontent.com/12954634/106982798-9b2a0e00-6719-11eb-8772-0492052d70c9.png">
    <figcaption align="center"> <i>No gap</i>
    </figcaption>
</figure>

I did some searching, it turns out lots of people have the same complain. All posts I found are like [this one](https://git.pwmt.org/pwmt/zathura/-/issues/116), posted years ago without having a solution. Although I was a bit disappointed, at least I know I won't be fixing an unexisted problem.

First get the source code from https://git.pwmt.org/pwmt/zathura. Zathura is written in c, built with `meson` and `ninja`. I've never used either of them, but comparing with `cmake` and `autotool`, the config file you write is just so much nicer. The source code is very clean, and different functionalities are clearly separated. It seems's the program depends on `girara`, another gui framework made by the author, which is itself based on `gtk`. I didn't have experience with either of them, so it is a good opportunity to play with something different.

#### OOP and GUI
`GTK` is a GUI library like `QT`. Although it's also cross-platform, it seems to be more popular on the linux world. GUI frameworks usually adopt object orientation heavily, because oop really makes sense in the context. Though you can have thousands of reasons to hate oop, I think GUI is one of the acutal legitimate application of the paradigm. For a GUI library, everything is a widget and they do some widget things. Some widgets are specialized for specific tasks, so they extend the functionality of the basic widget. The extensibility from subtyping and inheritence makes it easy to model the specific problem. Subtyping is for the partial order of types, inheritence is for method dispatching. Together they make it easy to extend a type but hard to extend methods[!](https://stackoverflow.com/questions/3271974/why-adts-are-good-and-inheritance-is-bad). For GUI problem, We want to be able to define different widgets, so the naturally the type should be as extensible as possible.

For a good comparison, the same problem is hard to express in algebraic data type from any typical ml languages. Algebraic data types are closed on extending the type, if you want to add new widget types, all functions that work with the type needs to be changed. Althogh you can simulate subtyping with typecalss and existential type in Haskell [!](https://news.ycombinator.com/item?id=4784116), But that just lead us cycle back to the same solution.

```haskell
```


because it's hard to achieve extensibility[!](https://www.andres-loeh.de/OpenDatatypes.pdf). An attempt to introduce subtyping to aglebraic data type doesn't have the full inheritence support from dynamic dispatching [!](https://folk.ntnu.no/haugwarb/Programming/OHaskell/o_haskell_tutorial.html#sect4)

#### GTK and gobject.


#### girara

#### Source code

#### Fix

#### Conclusion
Thank you for reading this totally off the topic diary-ish article. I was trying to be more to the point, but it turns  it's just a record of a random day me doing random thing, so really no point to begin with.
