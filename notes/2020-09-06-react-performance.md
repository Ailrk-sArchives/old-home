-- tag note react
-- title React internal and app performance.
-- date 2020-09-06
-- source https://medium.com/myheritage-engineering/how-to-greatly-improve-your-react-app-performance-e70f7cbbb5f6
          https://blog.reverberate.org/2014/02/react-demystified.html
          https://github.com/reactjs/react-basic/blob/1512678469e04da02fe052ba884480a78f2e03ee/README.md
          http://swannodette.github.io/2013/12/17/the-future-of-javascript-mvcs/
;;
## React

#### Goal
A UI library provides better abstraction model and mitigating the overhead of updating dom directly.

#### Basic structure.

Operations on dom is expensive, so instead of updating the dom directly, react maintain a virtual dom that can be mapped to the real dom, and only update the real dom when it's necessary.

Virtual Dom helps batch all the changes and perform the minimal amount of change possible based on the difference between changed virtual dom and the real dom in each rerender.

#### Virtual dom and state.

Virtual dom serves as a ir between react and real dom. Right after each render the virtual dom should be isomorphic to the real dom strcture.

```
|   | builds/modify->|           | builds/modify-> |   |
|App|                |Virtual DOM|                 |DOM|
|   |<-deliver events|           | <-deliver events|   |
```

Besides being able to represent the dom structure, virtual dom is also the one will get modified between each render, and it's the way your react app talk with the dom. All states and effects that will affect the dom structure are ornagized by hooks like `setState` and `usEffect`. For instance, whenever `setState` is triggered, react will make the change on the virtual dom correspondingly and then call a rerender.

#### Virtual Dom to Dom

React virtual dom is organization with many components, which corresponds to some element in the real dom. Different from the real dom, a virtual dom component also contains all the logic about the element.

The structure of the virtual dom is reflected on the code. If you're using hooks you are actually building up the virtual dom by hand. So your program structure is basically your dom structure. This is different from traditional MVC architecture which your controller and model can be totally inconsistent with the ui as long as data flows correctly.

Note: an additional benefit from using virtual dom is you can have some new html features or html extensions even (before they are available )[https://www.html5rocks.com/en/tutorials/webcomponents/shadowdom/]. This is the same as using babel to transform javascript.

##### Host tree

Besides browser dom, react can also target to other host platforms like ios and android trees. The target is usually called host tree, and elements on that tree is called host instances.

#### React state management

With virtual dom we can build the real dom with some template syntax, but that's not enough for a real app. To be interactive you also need to handle states and effects.

React is not strictly pure, so you can have local state as much as you want. But if a state change will require some rerendering of the UI, it needs to be managed with `useState`. `useState` return a state updater `setState`, whenever it is called, react knows some UI needs to be rerendered to synchronize with the new state.

In short

1. If no state change, the page is just static and no need to rerener.
2. If a `setState` is called, react knows some UI related state changed, and will rerendere the component where the `setState` is called.
3. prop will never change unless the parent rerender the component with a different prop.

#### Reconsiliation
React's job is to make the host tree match the provied virtual dom tree and the process of deciding what to do with the dom element based on the difference between it and the virtual dom is called reconsiliation.

Ideally a pure update replace the entire old data structure with a new one, but for a tree like browser dom this approach has several problems:

1. Destroy and recreate the entire tree is not performed enough.
2. If the dom tree is destroyed, the previous state like scroll, mouse position will also lost (This all stored at dom).

So in general react will only update the changed node and it's child nodes if necessary.

#### Handle input from the dom
You handle dom events by event handlers, the difference part in react is that instead of registering event handling to the real dom directly, all events in the real dom will propageted up to the top of the tree, and be dispatched to their corresponding handlers.

An event handler might trigger some app's state change (call `setState` etc). In that case react will trigger a rerender as described above.

## React app performance

My website has some serious layout thrashing while loading the note list, and I am trying to fix it.

#### Changing DOM too fast
Two common problems can be caused by creating complex components:

#### Triggering layout too often.

When you change css properties [these](https://csstriggers.com/) here, the browser will need to rerender the screen to keep the UI up to date. Browser update includes `Layout`, `Paing`, and `Composite`, different css properties will required different updates. For instance, change padding requires all three updates, that is, the browser first need to recalculate the layout of the dom element, repaint it back to screen, and composite the page together again. If this happen too often the rendering will feel very laggy.

###### Layout thrashing

This is caused by unnecessary DOM reflow. For exampe, reading from dom right after write to it.

For example, you want to change the height of an element. To achieve that you first read the height from dom, then increment the height. This cause layout thrashing because everytime you read the element you instantly change the dom, which requires a rerender.

###### Solution
Use `window.requestAnimationFrame((time: number) => void)`. It's original purpose is to execute an animation callback before the next repaint. What we can do is to use it to separate read and write into two different frames.

```typescript
updateHeight(isOpen) {
  this.lastRAF && cancelAnimationFrame(this.lastRAF);
  if (isOpen) {
    this.lastRAF = requestAnimationFrame(() => {
      //  Do all your read at this layer.
      const height = `${this.contentEl.scrollHeight}px`;
      this.lastRAF = requestAnimationFrame(() => {
        // write change in the next frame.
        this.contentEl.style.height = height;
        this.lastRAF = null;
      });
    })
  } else {
    this.contentEl.style.height = '0px';
  }
}
```

#### events and callbacks

You don't want a super lengthy callback on your event queue since it will block the entire stack. Try to separate a big callback into smaller ones, or

Another problem is events that happens frequently. For example, autocomplete when typing. Standard solution is to use a debounce function.

#### Turns out...
I solved it with a little delay before render the page... Works suprisingly well...
