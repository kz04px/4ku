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
3,432 bytes
```

---

## UCI Support
The UCI protocol isn't strictly followed in the interests of space:
- The first line of input after startup will be interpreted as "uci" even if it isn't.
- "stop" is unsupported.
- Both "position fen [fen]" and "position startpos" are ignored.
- Only wtime and btime are supported.
