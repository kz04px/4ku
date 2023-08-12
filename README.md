# 4ku
A chess engine written in C++ designed to fit into 4,096 bytes. There are two versions of the engine: 4ku, and 4ku-mini.

- 4ku-mini uses source code that is stripped, minified, compressed, and then appended to the launch script. When run, the launch script compiles the source code to a temporary executable and then runs it.

- 4ku is a normal compile of the same source code. It is not stripped so retains support for UCI `setoption`, info strings, and perhaps other quality of life improvements.

4ku and 4ku-mini should be identical in terms of their play, but 4ku's info strings mean it is probably slightly slower and slightly weaker. Despite this, 4ku's ease of use and cross-platform compatibility means it should probably be favoured for use in any circumstance other than being limited to 4,096 bytes.

---

## Build Instructions
4ku can be built normally, but it won't fit into the size restriction:
```
git clone https://github.com/kz04px/4ku
mkdir 4ku/build
cd 4ku/build
cmake ..
cmake --build .
```
To build 4ku-mini on Linux, run `build-mini.sh` located in the root directory:
```
bash build-mini.sh
```
4ku-mini is currently unavailable on Windows.

---

## 4ku-mini Size
```
3,873 bytes
```

---

## Requirements
4ku only needs a C++ compiler to be built and should work across platforms.
4ku-mini has the following additional requirements:
- python3
- lzma

The build script, launch scripts, and compression tool (lzma) are all specific to Linux and would need replacing for 4ku to run on Windows. The code itself should be portable.

---

## Minification
The minifier requires Python 3 to run. It's fragile and will not handle arbitrary C++ code. The minifier is not a general-purpose solution and is not guaranteed to work on non-4ku code. If better solutions are found in the future it can be replaced.

Removed:
- Whitespace
- Single line comments
- Block comments
- const
- noexcept
- Attributes: nodiscard, maybe_unused
- Calls to assert() and static_assert()
- enums

---

## UCI Support
4ku-mini has limited UCI support to save space:
- The first line of input after startup will be interpreted as `uci` even if it isn't.
- `stop` is unsupported.
- `position fen [fen] moves [moves]` is not supported. `position startpos moves [moves]` must be used instead.
- `go` only supports `wtime` and `btime`, in that order.
- `setoption` is not supported. Relevant variables are hard coded.

4ku has additional support for:
- `setoption`
- `position fen [fen] moves [moves]`
- `info` strings

---

## Thanks
- The people of the [Engine Programming Discord server](https://discord.gg/invite/YctB2p4) for their help and encouragement.
