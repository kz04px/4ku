# 4ku
4ku is an a C++ chess engine designed to fit into 4,096 bytes. Source code is minified, compressed, and then appended to a script. The script decompresses the source, compiles it, and then runs the executable. The script and source combination fits into the size limits, while the resulting executable does not.

---

## Build Instructions
```
git clone https://github.com/kz04px/4ku
cd 4ku
sh build.sh
```

---

## Size
```
3,575 bytes
```

---

## Requirements
- Linux
- A C++ compiler
- python3
- lzma

The build script, launch scripts, and compression tool (lzma) are all specific to Linux and would need replacing for 4ku to run on Windows. The code itself should be portable.

---

## Minification
The minifier requires Python 3 to run. It's fragile and will not handle arbitrary C++ code. If better solutions are found in the future it can be replaced.

Points of note:
- Variable name substitutions are hard coded
- Erroneous substitutions may happen
- Multiple passes required

Removed:
- Whitespace
- Single line comments
- Block comments
- const
- noexcept
- Attributes: nodiscard, maybe_unused

---

## UCI Support
The UCI protocol isn't strictly followed in the interests of space:
- The first line of input after startup will be interpreted as "uci" even if it isn't.
- "stop" is unsupported.
- Both "position fen [fen]" and "position startpos" are ignored.
- Only wtime and btime are supported.
