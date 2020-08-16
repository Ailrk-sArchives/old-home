-- tag note perf front-end lazyness
-- title Functional reactive programming primer
-- date 2020-08-14
-- source https://developers.google.com/web/fundamentals/performance/http2
          https://developers.google.com/web/fundamentals/performance/rendering/avoid-large-complex-layouts-and-layout-thrashing
          https://www.codementor.io/@agustinchiappeberrini/lazy-evaluation-and-javascript-a5m7g8gs3
;;
# Audit this website on chrome.
I feel my artile load time is pretty laggy, so I tried to play with chrome audit tools to see what's wrong. I feel this is a valuable perf experience and worth note down.

All perf stuffs are better done in incognito mode, otherwise the result will be strongly affected by the plugin you're using. It's also very hard to pick the perf data for your website and the perf data for the plugin, which make perf unnecessarily harder.

## Tools
#### Chrome Performance
It's the basic perf tool come with chrom for years. Record action for a period of time, and generate a flame chart based on the report from the browser and v8. You can assess what took the largest chunk of times to load. Common time consumer are script and layout.

#### React profile
React has it's specific perf tool, instead of giving a generic perf data from engines, it tells you how much time an individual component takes. This is especially helpful for pin down the spot in react app.

#### Audit
Google lighthouse tool, It gives some heuristic based website quality check of the overall performance of the website, and the report generated includes some advice you can follow along. Advices are in form of `don't put too many data in on transfer`, it's pretty straight forward.

## What I learnt

#### RAIL
User cenetric performance model. Bascially a bunch of heuristics that helps you to talk about the performance.

In large,there're four main aspects to assess. namely `Response`, `Animation`, `Idle`, `Load`. Based on the types of app, the performance goals can shift.

Some metrics (tolerances)

```
0 - 16 ms         smooth animatino (60 fps)
0 - 100 ms        response to user action.
100 - 1000ms      loading page or changing view
1000ms or more    beyond this point ppl lose focus
10000ms or more   ppl tend to abandon the task
```

#### Cache static resource with HTTP caching
Cache TTL can be set from the server HTTP response header. Like `Cache-Control: max-age=300000`. How long to set is totally depends on the data type. If it's immutable data, then it's ok to set to a long time. For volatile data it might have a shorter cache TTL or `no-chace`.

#### Avoid chaining critical requests
If too many requests jammed at the same time, the performance of course will be affected. But sometimes it's necessary to have that many requests to make the app work. If it's not possible to optimize the data itself, a way to make things better is to make the app process more lengthy, so it can have more opportunity to fetch data in between. Loading critical data at the beginning of the app also helps avoid jamming.

## What I did
The perf tool told me the loading time for list page is very high, it's because previously the artile list is sorted forevery single route. I changed that, and thing runs a bit faster.

But the real problem of feeling laggy is because the

#### True  lazyness
My `markdowndb.macro` macro tries to build a interface that can query static file in a lazy fasion. The initial load will only load headers of the file, and the content will be loaded whenever a `get` method is called on the interface. But after testing it is not really lazily evaluated, all html files are downloaded right in front of of the load.

My "thunk" is just `() -> fetcht(url)`, and whenever the app calls `get`, it will invoke the functinon.


#### Layout problem

## Conclusion
A perf really give a lot of insights of the program. It helps you find problem that you might never notice by just looking at the code.
