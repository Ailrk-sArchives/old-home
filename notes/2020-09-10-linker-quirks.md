-- tag note linux linker so
-- title Parsing
-- date 2020-09-10
-- source https://datacadamia.com/os/linux/ld_library_path#:~:text=LD_LIBRARY_PATH%20is%20the%20search%20path,the%20standard%20set%20of%20directories.
          https://codeyarns.com/2017/11/02/how-shared-library-locations-are-found-at-runtime/
          https://tldp.org/HOWTO/Program-Library-HOWTO/shared-libraries.html
          https://unix.stackexchange.com/questions/120015/how-to-find-out-the-dynamic-libraries-executables-loads-when-run
;;
# Some linker stuffs

You can check the dependency of a binary with `ldd`, and when you actually run the executable, the share object will be linked by `ld`.

## ld
ld combines a number of objecets and archive files and tie up symbol references. First thing to know is `ld` is not only for c and cpp, it's just a generic program for linking object files. As long as object files follow formats it support.

## ldd
Print the shared objects required by each program specified on the command line.

## ldconfig
Configure dynamic linker rutime binding.

#### ldconfig -p
`ldconfig -p` shows the current binding. This list indicates if your linker can find a given package. If your shared object is not on the list, you might need to add it to the path.

Sometimes `ldd` shows a dependencies's location, but if that location is not on `ldconfig -p` it's still not linkable.

#### ldconfig -n <path-to-lib>
If you have a shared library somewhere, but it's not available at link stage, the easiest way to make it work is to do `ldconfig -n <path>`.


## LD_LIBRARY_PATH=<path>
The enviroment variable is used to temporarily substitute a different library for a particular execution.

It is handy for development and testing, but binary release should not depend on it.

## LD_DEBUG=<[files|libs|bindings]>
Environment variable to trigger more verbose logging during linking.

## readelf
`readelf`is another alternative for `ldd`. You can do `readelf -d <libname> | grep NEEDED` to check required dynamic libraries.


## Note for gcc
If you are using gcc to make a shared library, the compiling process looks like this:

```
gcc -fPIC -g -c -Wall a.c
gcc -fPIC -g -c -Wall b.c
gcc -shared -Wl, -soname, libmystaff.so.1 \
    -o libmystaff.so.1.0.1 a.o b.o -lc
```

The first two lines use `-c` to specifies not to run the linker, so we can produce stand alone object files. `-fPIC` is used to generate positino indepedent code. This is of course the first step to make the library shareable.
