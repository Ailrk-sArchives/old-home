-- tag note react
-- title React app performance.
-- date 2020-09-06
-- source https://medium.com/myheritage-engineering/how-to-greatly-improve-your-react-app-performance-e70f7cbbb5f6
;;
# React app performance

My website has some serious layout thrashing while loading the note list, and I am trying to fix it.

## Changing DOM too fast
Two common problems can be caused by creating complex components:

#### Triggering layout too often.

When you change css properties [these](https://csstriggers.com/) here, the browser will need to rerender the screen to keep the UI up to date. Browser update includes `Layout`, `Paing`, and `Composite`, different css properties will required different updates. For instance, change padding requires all three updates, that is, the browser first need to recalculate the layout of the dom element, repaint it back to screen, and composite the page together again. If this happen too often the rendering will feel very laggy.

#### Layout thrashing

This is caused by unnecessary DOM reflow. For exampe, reading from dom right after write to it.

For example, you want to change the height of an element. To achieve that you first read the height from dom, then increment the height. This cause layout thrashing because everytime you read the element you instantly change the dom, which requires a rerender.

#### Solution
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

## events and callbacks

You don't want a super lengthy callback on your event queue since it will block the entire stack. Try to separate a big callback into smaller ones, or

Another problem is events that happens frequently. For example, autocomplete when typing. Standard solution is to use a debounce function.
