# 4ku2
A variation of the chess engine [4ku](https://github.com/kz04px/4ku/), but this time it works by compiling source on startup to further minimise file size.

---

## Build Instructions
```
git clone https://github.com/kz04px/4ku2
cd 4ku2
sh build.sh
```

---

## Size
```
3,494 bytes
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
