
# Gong

Gong is a language for Collision Detection and other Spatial Join problems in Graphics.

## Quick Setup

### Terra
Make sure you have Terra installed (TODO: better details here).  And make sure that Terra is on your system `PATH`.

Add the [`bin/`](bin) directory to your `PATH` environment as well, so that the [`bin/gong`](bin/gong) script is accessible.  To run a Gong program named `hello42.t`, you can now just execute
```gong hello42.t```

TODO: have an actual hello world program that can be run.

## More Details

Some of the examples and tests emit debug visuals, which can be viewed by running an instance of [vdb](https://github.com/zdevito/vdb).

### CUDA Support
Gong includes support for generating CUDA code.  However, the Terra installation must first have CUDA support.

TODO: a Manual

## Examples

TODO

## Tests

The development test suite can be run by executing
```
./runtests
```