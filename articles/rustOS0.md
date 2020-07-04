-- tag rust OS riscv
-- source xv6-reference
-- title Replicate xv6 on rust #0 Table of content
-- time 2020-07-03
;;
# Replicate xv6 on rust #0 Table of content
This is a series intents to replicate famous [xv6](https://github.com/mit-pdos/xv6-public) -- a simple UNIX like operating system, with rust. I have been thought about doing it for a long, long time, but never had enough incentive to take the action. Thanks to covid-19 I will be stuck at my room for at least 2 month, and I think it's about time to do it!

Xv6 is a pedagogical operating system for a MIT undergraduate course. Historically it was written for x86 architecture, but now it is switched to [RISC-V](https://en.wikipedia.org/wiki/RISC-V), an ISA shares some similarities with [MIPS](https://en.wikipedia.org/wiki/MIPS_architecture). The C code is largely unchanged, and RISC-V is one of the [official target](https://github.com/rust-embedded/riscv) provided by `rustc`, which one to choose is just a matter of preference. We a going to use RISC-V in this series for no particular reason.

The first principle of an operating system is to provide a easy to use interface for hardwares. This is a very generic goal, and to me, all other functionalities can be regarded as an extension of it. If you take a look at what comprise an operating system, those components are seemingly arbitrary: processes, file system, drivers, etc, which has little to do with each other (logically). While if you take a look at a compiler, it's subsystem are all chained together tightly with a more specific goal -- to transform some form of text to another. Because the goal of operating system is so generic, any handy abstractions over hardware can be considered to be part of it; and those components we see in Unix today are some well studied, well constructed ones that shapes the way we use computer.

This series will have multiple articles. Here is a table of them (updating):
* [RISCV overview](http://www.url.com)
