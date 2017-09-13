# zmq-protobuf-toy-app
Toy app: using zmq + protobuf to communicate between c++ and haskell.


# What it does.

This app consists from client and server.

Client sends the following requests to server:
* `ECHO <string>`: server replies with the same string.
* `READ <filename>`: server replies with file contents.
* `WRITE <filename> <contents>`: server writes given contents to file.

The user can run `c++` or `haskell` client or server. The `haskell` client is not fully implemented. I'm too lazy to make `c++` analogue.

# How to run.

Both `c++` and `haskell` binary are called `app`.

Call `app 0` to run server or `app 1` to run client.

The `haskell` `app` tells how to run it. I didn't the same it in `c++`.

# How to build.

To build `c++` part, just use `make`. `make` call `protoc` compiler. So it seems to work fine.

To build `haskell` part, use `hprotoc` first. I didn't add it to `build-tools` to `.cabal`.
```sh
hprotoc --proto_path=. --haskell_out=hs messge.proto
```
Then call `stack build`.

One can get `hprotoc` from [here](https://github.com/k-bx/protocol-buffers) (github repo).
