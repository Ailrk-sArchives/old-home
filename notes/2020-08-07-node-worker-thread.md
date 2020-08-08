-- tag note nodejs concurrency
-- title Worker thread
-- date 2020-08-07
-- source https://blog.insiderattack.net/deep-dive-into-worker-threads-in-node-js-e75e10546b11
          https://nodejs.org/api/worker_threads.html
;;
# Worker threads in node

## Pig picture
Worker threads are actually isolated v8 instance. Each worker thread will have its own event loop. The main difference from multiprocessing is you can have shared memory with buffer array, but for process you can only use IPC.

Each worker thread has it's own isolated v8 environment, and its execution will be isolated from other worker threads. The only way to communicate is via the shared memory with the main thread.

Worker threads runs worker scripts, which can be both another file (passed as filename) or source code in text.

Parent worker and child workers are connected via a `message channel`. It's like a pipe implementation with two ends.

## V8 isolation
#### Isolated instance.
V8 [isolations](https://v8docs.nodesource.com/node-0.8/d5/dda/classv8_1_1_isolate.html) are separated from each other, and each isolation has its own heap and microtask queue. Not like in c++ threads can access whatever, by default there is nothing shared between two isolations.

#### Interleave with c++ implementation

