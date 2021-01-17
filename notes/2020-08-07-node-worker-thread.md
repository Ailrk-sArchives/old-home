-- tag note nodejs concurrency
-- title Worker thread
-- date 2020-08-07
-- source https://blog.insiderattack.net/deep-dive-into-worker-threads-in-node-js-e75e10546b11
          https://nodejs.org/api/worker_threads.html
;;
# Worker threads in node

## Big picture
Worker threads are actually isolated v8 instance. Each worker thread will have its own event loop. The main difference from multiprocessing is you can have shared memory with buffer array, but for process you can only use IPC.

Worker threads runs worker scripts, which can be both another file (passed as filename) or source code in text.

Each worker thread has it's own isolated v8 environment, and its execution will be isolated from other worker threads. The only way to communicate is to share memory with the main thread (by messge channel).

## V8 isolation
#### Isolated instance.
V8 [isolations](https://v8docs.nodesource.com/node-0.8/d5/dda/classv8_1_1_isolate.html) are separated from each other, and each isolation has its own heap and microtask queue. Not like in c++ threads can access whatever, by default there is nothing shared between two isolations.

#### Implementation
V8 isolates are isolate from each other, so to achieve communication across parent and child worker threads will requires some extra help from underlying c++ api.  Creation of wroker threads involve two steps, worker initialization and worker execution.

During worker initialization worker instances are initialized, and the first communication between parent and child workers is set up to allows passing essential config data from parent to child.

In worker exection, worker script will be executed with the access of two things: `workerData` provided by user; config data sent by the parent thread worker during the first communication.

```
worker thread implementation                                Userland
============================================================<
Parent Js script                          Worker Js Script
==============↑===========================↑=================< Node
Node Workers  |                           | Node Worker
initilization +==public message channel===+  Execution
   script                                     script
=====↓===========================================↑==========<
     +--------initialization message channel-----+  c++ worker
                       worker.cc                    implementation
============================================================
```

##### Initialization steps
1. New Worker instance from `worker_threads`
2. Parent initilization script calls into c++, which will create an empty worker object.
3. The worker object will generate a thread id to itself.
4. Create an empty initilization message channel in c++.
5. Create a public message channel in javascript side.
6. The parent worker calls worker.cc, send initialization metadata to initialization message channel.

##### Execution steps.
1. Create and a new v8 isolate to the worker.
2. Initialize libuv for the v8 isolate so the worker can have its own event loops.
3. Execute worker execution script.
4. Worker execution script calls into c++, get the metadata send from parent worker thread in the initialization step.
5. Worker execution script execute the work (file or code).

#### Worker thread pooling
Like a normal thread, pooling can help reduce the overhead of frequently spawn new threads.

## apis.
#### class: MessageChannel
An asychronous two way communication channel (public channel described above). It has two ports `port1` and `port2`, which links to `MessagePort` instances.

#### class: MessagePort extends EventEmitter
Represent one end of the `MessageChannel`. It's the instance will be used to send and receive data.

##### Events of MessagePort
- 'close' emitted once either side of the channel has been disconnected.
- 'message' for incoming message
- 'messageerror' when deserializing message failed.

##### port.close()
Emit `close` event on both `MessagePort`.

##### port.postMessage(value[,transferlist])
Value be sent is compatible with `structured clone algorithm`

- `transferlist` can be a list of `ArrayBuffer`, `MessagePort`, and `FileHandle` objects.
- After transfering, data in `transferlist` will no longer be able to be used on the sending side (moved).
- Shared memory like `SharedArrayBuffer` can not be listed in `transferlist`, because it can be accessed by both sides of the `MessageChannel`.
- An argument that is not in the transferlist will be copied.

##### port.ref(), port.unref()
`unref` will allow the thread to exit if it is the only active handle on the event system.
`ref` is the opposite of `ref`, and it will automatically be called once `message` listener is attached.

##### `Notes` for transferring TypedArrays and Buffers.
Be careful when transferring `ArrayBuffer`. If an `TypedArray` is accessed after it's corresponding `ArrayBuffer` is transferred, it will be a dangling pointer.

An `ArrayBuffer` can be marked with markAsUntransferable, doing so will ensure it can only be cloned.

#### Worker
Mainly some options on the constructors and some events. Just check the doc.

#### others
##### markAsUntransferable(obj)
The marked obj will ignored if it occurs in port.postMessage(). (post by copy rather than move).

##### moveMessagePortToContext(port, contextifiedSandbox)
Swith the `MessagePort` to a different vm Context.

##### parentPort
Alias for `MessagePort` on the parent end.

##### receiveMessageOnPort(port)
Receive one message from a give `MessagePort`. `message` event will not be emitted if this function is used.

##### resourceLimits
```
resourceLimits = {
    maxYoungGenerationSizeMb,
    maxOldGenerationSizeMb,
    codeRangeSizeMb,
    stackSizeMb,
}
```
These are good to know information for child worker threads. (In main thread this will return an empty object)

##### threadId
Unique in a single proces.

##### workerData
The data is cloned

## Dos and Donts
