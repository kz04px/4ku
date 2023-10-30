// minify enable filter const
// minify enable filter line_comment
// minify enable filter block_comment
// minify enable filter nodiscard
// minify enable filter noexcept
// minify enable filter empty_lines
// minify enable filter trailing_newline
// minify enable filter trailing_whitespace
// minify enable filter whitespace
// minify enable filter enum

// minify disable function assert
// minify disable function static_assert

// minify replace true 1
// minify replace false 0
// minify replace NULL 0

#include <array>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <ctime>
#include <iostream>
#include <random>
#include <string>
#include <thread>
#include <vector>
// minify enable filter delete
#include <sstream>
// minify disable filter delete

using namespace std;

using i32 = int;
using u64 = uint64_t;

// Constants
const i32 mate_score = 1 << 15;
const i32 inf = 1 << 16;

enum
{
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
    None
};

[[nodiscard]] u64 now() {
    timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec * 1000 + t.tv_nsec / 1000000;
}

struct [[nodiscard]] Position {
    array<i32, 4> castling = {true, true, true, true};
    array<u64, 2> colour = {0xFFFFULL, 0xFFFF000000000000ULL};
    array<u64, 6> pieces = {0xFF00000000FF00ULL,
                            0x4200000000000042ULL,
                            0x2400000000000024ULL,
                            0x8100000000000081ULL,
                            0x800000000000008ULL,
                            0x1000000000000010ULL};
    u64 ep = 0x0ULL;
    i32 flipped = false;
};

struct Move {
    i32 from = 0;
    i32 to = 0;
    i32 promo = 0;
};

const Move no_move{};

struct [[nodiscard]] Stack {
    Move moves[256];
    Move quiets_evaluated[256];
    int64_t move_scores[256];
    Move move;
    Move killer;
    i32 score;
};

// Static eval using the TT and TT cutoffs rely on this specific ordering, do not change it.
enum
{
    Upper,
    Lower,
    Exact
};

struct [[nodiscard]] TT_Entry {
    u64 key;
    Move move;
    i32 score;
    i32 depth;
    uint16_t flag;
};

u64 keys[848];

// Engine options
u64 num_tt_entries = 64ULL << 15;  // The first value is the size in megabytes
i32 thread_count = 1;

vector<TT_Entry> transposition_table;

[[nodiscard]] u64 flip(const u64 bb) {
    return __builtin_bswap64(bb);
}

[[nodiscard]] i32 lsb(const u64 bb) {
    return __builtin_ctzll(bb);
}

[[nodiscard]] i32 count(const u64 bb) {
    return __builtin_popcountll(bb);
}

[[nodiscard]] u64 east(const u64 bb) {
    return bb << 1 & ~0x0101010101010101ULL;
}

[[nodiscard]] u64 west(const u64 bb) {
    return bb >> 1 & ~0x8080808080808080ULL;
}

[[nodiscard]] u64 north(const u64 bb) {
    return bb << 8;
}

[[nodiscard]] u64 south(const u64 bb) {
    return bb >> 8;
}

[[nodiscard]] u64 nw(const u64 bb) {
    return north(west(bb));
}

[[nodiscard]] u64 ne(const u64 bb) {
    return north(east(bb));
}

[[nodiscard]] u64 sw(const u64 bb) {
    return south(west(bb));
}

[[nodiscard]] u64 se(const u64 bb) {
    return south(east(bb));
}

[[nodiscard]] i32 operator==(const Move &lhs, const Move &rhs) {
    return !memcmp(&rhs, &lhs, sizeof(Move));
}

[[nodiscard]] string move_str(const Move &move, const i32 flip) {
    string str;
    str += 'a' + move.from % 8;
    str += '1' + (move.from / 8 ^ 7 * flip);
    str += 'a' + move.to % 8;
    str += '1' + (move.to / 8 ^ 7 * flip);
    if (move.promo != None)
        str += "nbrq"[move.promo - Knight];
    return str;
}

[[nodiscard]] i32 piece_on(const Position &pos, const i32 sq) {
    const u64 bb = 1ULL << sq;
    for (i32 i = 0; i < 6; ++i)
        if (pos.pieces[i] & bb)
            return i;
    return None;
}

void flip(Position &pos) {
    pos.colour[0] = flip(pos.colour[0]);
    pos.colour[1] = flip(pos.colour[1]);
    for (i32 i = 0; i < 6; ++i)
        pos.pieces[i] = flip(pos.pieces[i]);
    pos.ep = flip(pos.ep);
    swap(pos.colour[0], pos.colour[1]);
    swap(pos.castling[0], pos.castling[2]);
    swap(pos.castling[1], pos.castling[3]);
    pos.flipped = !pos.flipped;
}

template <typename F>
[[nodiscard]] u64 ray(const i32 sq, const u64 blockers, F f) {
    u64 mask = f(1ULL << sq);
    mask |= f(mask & ~blockers);
    mask |= f(mask & ~blockers);
    mask |= f(mask & ~blockers);
    mask |= f(mask & ~blockers);
    mask |= f(mask & ~blockers);
    mask |= f(mask & ~blockers);
    return mask;
}

u64 diag_mask[64];

[[nodiscard]] u64 xattack(const i32 sq, const u64 blockers, const u64 dir_mask) {
    return dir_mask & ((blockers & dir_mask) - (1ULL << sq) ^ flip(flip(blockers & dir_mask) - flip(1ULL << sq)));
}

[[nodiscard]] u64 bishop(const i32 sq, const u64 blockers) {
    return xattack(sq, blockers, diag_mask[sq]) | xattack(sq, blockers, flip(diag_mask[sq ^ 56]));
}

[[nodiscard]] u64 rook(const i32 sq, const u64 blockers) {
    return xattack(sq, blockers, 1ULL << sq ^ 0x101010101010101ULL << sq % 8) | ray(sq, blockers, east) |
           ray(sq, blockers, west);
}

[[nodiscard]] u64 knight(const i32 sq, const u64) {
    const u64 bb = 1ULL << sq;
    return (bb << 15 | bb >> 17) & 0x7F7F7F7F7F7F7F7FULL | (bb << 17 | bb >> 15) & 0xFEFEFEFEFEFEFEFEULL |
           (bb << 10 | bb >> 6) & 0xFCFCFCFCFCFCFCFCULL | (bb << 6 | bb >> 10) & 0x3F3F3F3F3F3F3F3FULL;
}

[[nodiscard]] u64 king(const i32 sq, const u64) {
    const u64 bb = 1ULL << sq;
    return bb << 8 | bb >> 8 | (bb >> 1 | bb >> 9 | bb << 7) & 0x7F7F7F7F7F7F7F7FULL |
           (bb << 1 | bb << 9 | bb >> 7) & 0xFEFEFEFEFEFEFEFEULL;
}

[[nodiscard]] auto is_attacked(const Position &pos, const i32 sq, const i32 them = true) {
    const u64 bb = 1ULL << sq;
    const u64 pawns = pos.colour[them] & pos.pieces[Pawn];
    const u64 pawn_attacks = them ? sw(pawns) | se(pawns) : nw(pawns) | ne(pawns);
    return pawn_attacks & bb || pos.colour[them] & pos.pieces[Knight] & knight(sq, 0) ||
           bishop(sq, pos.colour[0] | pos.colour[1]) & pos.colour[them] & (pos.pieces[Bishop] | pos.pieces[Queen]) ||
           rook(sq, pos.colour[0] | pos.colour[1]) & pos.colour[them] & (pos.pieces[Rook] | pos.pieces[Queen]) ||
           king(sq, 0) & pos.colour[them] & pos.pieces[King];
}

auto makemove(Position &pos, const Move &move) {
    const i32 piece = piece_on(pos, move.from);
    const i32 captured = piece_on(pos, move.to);
    const u64 to = 1ULL << move.to;
    const u64 from = 1ULL << move.from;
    const u64 mask = from | to;

    // Move the piece
    pos.colour[0] ^= mask;
    pos.pieces[piece] ^= mask;

    // En passant
    if (piece == Pawn && to == pos.ep) {
        pos.colour[1] ^= to >> 8;
        pos.pieces[Pawn] ^= to >> 8;
    }

    pos.ep = 0;

    // Pawn double move
    if (piece == Pawn && move.to - move.from == 16)
        pos.ep = to >> 8;

    // Captures
    if (captured != None) {
        pos.colour[1] ^= to;
        pos.pieces[captured] ^= to;
    }

    // Castling
    if (piece == King) {
        const u64 bb = move.to - move.from == 2 ? 0xa0ULL : move.to - move.from == -2 ? 0x9ULL : 0x0ULL;
        pos.colour[0] ^= bb;
        pos.pieces[Rook] ^= bb;
    }

    // Promotions
    if (piece == Pawn && move.to >= 56) {
        pos.pieces[Pawn] ^= to;
        pos.pieces[move.promo] ^= to;
    }

    // Update castling permissions
    pos.castling[0] &= !(mask & 0x90ULL);
    pos.castling[1] &= !(mask & 0x11ULL);
    pos.castling[2] &= !(mask & 0x9000000000000000ULL);
    pos.castling[3] &= !(mask & 0x1100000000000000ULL);

    flip(pos);

    // Return move legality
    return !is_attacked(pos, lsb(pos.colour[1] & pos.pieces[King]), false);
}

void generate_pawn_moves(Move *const movelist, i32 &num_moves, u64 to_mask, const i32 offset) {
    while (to_mask) {
        const i32 to = lsb(to_mask);
        to_mask &= to_mask - 1;
        if (to >= 56) {
            movelist[num_moves++] = Move{to + offset, to, Queen};
            movelist[num_moves++] = Move{to + offset, to, Rook};
            movelist[num_moves++] = Move{to + offset, to, Bishop};
            movelist[num_moves++] = Move{to + offset, to, Knight};
        } else
            movelist[num_moves++] = Move{to + offset, to, None};
    }
}

template <typename F>
void generate_piece_moves(Move *const movelist,
                          i32 &num_moves,
                          const Position &pos,
                          const i32 piece,
                          const u64 to_mask,
                          F f) {
    u64 copy = pos.colour[0] & pos.pieces[piece];
    while (copy) {
        const i32 fr = lsb(copy);
        copy &= copy - 1;
        u64 moves = f(fr, pos.colour[0] | pos.colour[1]) & to_mask;
        while (moves) {
            const i32 to = lsb(moves);
            moves &= moves - 1;
            movelist[num_moves++] = Move{fr, to, None};
        }
    }
}

[[nodiscard]] i32 movegen(const Position &pos, Move *const movelist, const i32 only_captures) {
    i32 num_moves = 0;
    const u64 all = pos.colour[0] | pos.colour[1];
    const u64 to_mask = only_captures ? pos.colour[1] : ~pos.colour[0];
    const u64 pawns = pos.colour[0] & pos.pieces[Pawn];
    generate_pawn_moves(movelist, num_moves, north(pawns) & ~all & (only_captures ? 0xFF00000000000000ULL : ~0ULL), -8);
    if (!only_captures)
        generate_pawn_moves(movelist, num_moves, north(north(pawns & 0xFF00ULL) & ~all) & ~all, -16);
    generate_pawn_moves(movelist, num_moves, nw(pawns) & (pos.colour[1] | pos.ep), -7);
    generate_pawn_moves(movelist, num_moves, ne(pawns) & (pos.colour[1] | pos.ep), -9);
    generate_piece_moves(movelist, num_moves, pos, Knight, to_mask, knight);
    generate_piece_moves(movelist, num_moves, pos, Bishop, to_mask, bishop);
    generate_piece_moves(movelist, num_moves, pos, Rook, to_mask, rook);
    generate_piece_moves(movelist, num_moves, pos, Queen, to_mask, rook);
    generate_piece_moves(movelist, num_moves, pos, Queen, to_mask, bishop);
    generate_piece_moves(movelist, num_moves, pos, King, to_mask, king);
    if (!only_captures && pos.castling[0] && !(all & 0x60ULL) && !is_attacked(pos, 4) && !is_attacked(pos, 5))
        movelist[num_moves++] = Move{4, 6, None};
    if (!only_captures && pos.castling[1] && !(all & 0xEULL) && !is_attacked(pos, 4) && !is_attacked(pos, 3))
        movelist[num_moves++] = Move{4, 2, None};
    return num_moves;
}

[[nodiscard]] i32 S(const i32 mg, const i32 eg) {
    return (eg << 16) + mg;
}

const i32 phases[] = {0, 1, 1, 2, 4, 0};
const i32 max_material[] = {139, 449, 452, 841, 1674, 0, 0};
const i32 material[] = {S(100, 139), S(329, 449), S(341, 452), S(455, 841), S(825, 1674), 0};
const i32 pst_rank[] = {
    0,         S(-2, 0),  S(-3, -1), S(-1, -1), S(1, 0),  S(5, 2), 0,        0,          // Pawn
    S(-4, -5), S(-2, -3), S(-1, -1), S(1, 3),   S(3, 4),  S(7, 1), S(5, 0),  S(-11, 1),  // Knight
    S(-2, -2), S(1, -2),  S(1, 0),   S(1, 0),   S(2, 1),  S(4, 0), S(1, 0),  S(-8, 2),   // Bishop
    S(-2, -4), S(-2, -4), S(-3, -2), S(-4, 1),  S(-1, 2), S(3, 2), S(3, 3),  S(6, 1),    // Rook
    S(1, -13), S(1, -10), S(0, -5),  S(-1, 1),  S(-1, 6), S(1, 5), S(-2, 9), S(1, 7),    // Queen
    S(0, -6),  S(0, -2),  S(-2, 0),  S(-4, 3),  S(-1, 4), S(5, 4), S(3, 3),  S(4, -4)    // King
};
const i32 pst_file[] = {
    S(-2, 1),  S(-1, 1),  S(-1, 0), S(0, -1), S(1, 0),  S(2, 0),  S(2, 0),  S(-1, -1),  // Pawn
    S(-4, -3), S(-2, -1), S(0, 1),  S(2, 3),  S(2, 2),  S(2, 0),  S(1, -1), S(-1, -3),  // Knight
    S(-2, 0),  0,         S(1, 0),  S(0, 1),  S(0, 1),  S(-1, 1), S(2, -1), S(0, -1),   // Bishop
    S(-2, 0),  S(-2, 1),  S(-1, 1), S(1, 0),  S(1, -1), S(1, 0),  S(2, 0),  S(-1, -1),  // Rook
    S(-3, -4), S(-2, -2), S(-1, 0), S(0, 1),  S(0, 2),  S(1, 3),  S(2, 1),  S(3, -1),   // Queen
    S(-2, -5), S(2, -1),  S(-2, 1), S(-3, 2), S(-4, 2), S(-1, 1), S(2, -1), S(0, -5)    // King
};
const i32 open_files[] = {
    // Semi open files
    S(3, 4),
    S(-4, 20),
    S(20, 16),
    S(3, 19),
    S(-23, 10),
    // Open files
    S(-3, -12),
    S(-11, 0),
    S(47, 0),
    S(-13, 35),
    S(-63, 1),
};
const i32 mobilities[] = {S(9, 5), S(8, 7), S(4, 4), S(4, 3), S(-5, -1)};
const i32 pawn_protection[] = {S(24, 14), S(2, 16), S(8, 17), S(9, 8), S(-5, 23), S(-34, 26)};
const i32 passers[] = {S(0, 15), S(29, 52), S(63, 126), S(209, 210)};
const i32 pawn_passed_protected = S(13, 20);
const i32 pawn_doubled_penalty = S(14, 38);
const i32 pawn_phalanx = S(13, 12);
const i32 pawn_passed_blocked_penalty[] = {S(10, 14), S(-5, 43), S(-9, 86), S(3, 101)};
const i32 pawn_passed_king_distance[] = {S(1, -6), S(-4, 11)};
const i32 bishop_pair = S(31, 72);
const i32 king_shield[] = {S(35, -12), S(28, -8)};
const i32 pawn_attacked_penalty[] = {S(64, 14), S(155, 142)};

[[nodiscard]] i32 eval(Position &pos) {
    // Include side to move bonus
    i32 score = S(28, 10);
    i32 phase = 0;

    for (i32 c = 0; c < 2; ++c) {
        // our pawns, their pawns
        const u64 pawns[] = {pos.colour[0] & pos.pieces[Pawn], pos.colour[1] & pos.pieces[Pawn]};
        const u64 protected_by_pawns = nw(pawns[0]) | ne(pawns[0]);
        const u64 attacked_by_pawns = se(pawns[1]) | sw(pawns[1]);
        const i32 kings[] = {lsb(pos.colour[0] & pos.pieces[King]), lsb(pos.colour[1] & pos.pieces[King])};

        // Bishop pair
        if (count(pos.colour[0] & pos.pieces[Bishop]) == 2)
            score += bishop_pair;

        // Doubled pawns
        score -= pawn_doubled_penalty * count((north(pawns[0]) | north(north(pawns[0]))) & pawns[0]);

        // Phalanx pawns
        score += pawn_phalanx * count(west(pawns[0]) & pawns[0]);

        // For each piece type
        for (i32 p = 0; p < 6; ++p) {
            u64 copy = pos.colour[0] & pos.pieces[p];
            while (copy) {
                const i32 sq = lsb(copy);
                copy &= copy - 1;

                // Material
                phase += phases[p];
                score += material[p];

                const i32 rank = sq / 8;
                const i32 file = sq % 8;

                // Split quantized PSTs
                score += pst_rank[p * 8 + rank] * 8;
                score += pst_file[p * 8 + file] * 8;

                // Pawn protection
                const u64 piece_bb = 1ULL << sq;
                if (piece_bb & protected_by_pawns)
                    score += pawn_protection[p];

                if (p == Pawn) {
                    // Passed pawns
                    if (rank > 2 && !(0x101010101010101ULL << sq & (pawns[1] | attacked_by_pawns))) {
                        score += passers[rank - 3];

                        // Protected passed pawns
                        if (piece_bb & protected_by_pawns)
                            score += pawn_passed_protected;

                        // Blocked passed pawns
                        if (north(piece_bb) & pos.colour[1])
                            score -= pawn_passed_blocked_penalty[rank - 3];

                        // King defense/attack
                        // king distance to square in front of passer
                        for (i32 i = 0; i < 2; ++i)
                            score += pawn_passed_king_distance[i] * (rank - 1) *
                                     max(abs(kings[i] / 8 - rank - 1), abs(kings[i] % 8 - file));
                    }
                } else {
                    // Pawn attacks
                    if (piece_bb & attacked_by_pawns)
                        // If we're to move, we'll just lose some options and our tempo.
                        // If we're not to move, we lose a piece?
                        score -= pawn_attacked_penalty[c];

                    u64 mobility = 0;

                    // Rook, Queen, King
                    if (p > Bishop)
                        mobility = rook(sq, pos.colour[0] | pos.colour[1]);

                    // Knight
                    if (p == Knight)
                        mobility = knight(sq, pos.colour[0] | pos.colour[1]);

                    // Bishop, Queen, King
                    else if (p != Rook)
                        mobility |= bishop(sq, pos.colour[0] | pos.colour[1]);

                    // Use Queen mobilities for the king as a form of king safety.
                    // Don't consider squares attacked by opponent pawns.
                    score += mobilities[p - 1] * count(mobility & ~pos.colour[0] & ~attacked_by_pawns);

                    // Open or semi-open files
                    const u64 file_bb = 0x101010101010101ULL << file;
                    if (!(file_bb & pawns[0]))
                        score += open_files[!(file_bb & pawns[1]) * 5 + p - 1];

                    if (p == King && piece_bb & 0xC3D7) {
                        // C3D7 = Reasonable king squares
                        // Pawn cover is fixed in position, so it won't
                        // walk around with the king.
                        const u64 shield = 0x700 << 5 * (file > 2);
                        score += count(shield & pawns[0]) * king_shield[0];
                        score += count(north(shield) & pawns[0]) * king_shield[1];
                    }
                }
            }
        }

        flip(pos);

        score = -score;
    }

    // Tapered eval with endgame scaling based on remaining pawn count of the stronger side
    return (int16_t(score) * phase +
            (score + 0x8000 >> 16) * (16 + count(pos.colour[score < 0] & pos.pieces[Pawn])) / 24 * (24 - phase)) /
           24;
}

[[nodiscard]] u64 get_hash(const Position &pos) {
    u64 hash = pos.flipped;

    // Pieces
    for (i32 p = Pawn; p < None; ++p) {
        u64 copy = pos.pieces[p] & pos.colour[0];
        while (copy) {
            const i32 sq = lsb(copy);
            copy &= copy - 1;
            hash ^= keys[p * 64 + sq];
        }
        copy = pos.pieces[p] & pos.colour[1];
        while (copy) {
            const i32 sq = lsb(copy);
            copy &= copy - 1;
            hash ^= keys[(p + 6) * 64 + sq];
        }
    }

    // En passant square
    if (pos.ep)
        hash ^= keys[768 + lsb(pos.ep)];

    // Castling permissions
    hash ^= keys[832 + (pos.castling[0] | pos.castling[1] << 1 | pos.castling[2] << 2 | pos.castling[3] << 3)];

    return hash;
}

i32 alphabeta(Position &pos,
              i32 alpha,
              const i32 beta,
              i32 depth,
              const i32 ply,
              // minify enable filter delete
              u64 &nodes,
              // minify disable filter delete
              const int64_t stop_time,
              i32 &stop,
              Stack *const stack,
              int64_t (&hh_table)[2][64][64],
              vector<u64> &hash_history,
              const i32 do_null = true) {
    // Don't overflow the stack
    if (ply > 127)
        return eval(pos);

    // Check extensions
    const i32 in_check = is_attacked(pos, lsb(pos.colour[0] & pos.pieces[King]));
    depth += in_check;

    i32 in_qsearch = depth <= 0;
    const u64 tt_key = get_hash(pos);

    if (ply > 0 && !in_qsearch) {
        // Repetition detection
        for (const u64 old_hash : hash_history)
            if (old_hash == tt_key)
                return 0;
    }

    // TT Probing
    TT_Entry &tt_entry = transposition_table[tt_key % num_tt_entries];
    Move tt_move{};
    if (tt_entry.key == tt_key) {
        tt_move = tt_entry.move;
        if (alpha == beta - 1 && tt_entry.depth >= depth && tt_entry.flag != tt_entry.score <= alpha)
            // If tt_entry.score <= alpha, tt_entry.flag cannot be Lower (ie must be Upper or Exact).
            // Otherwise, tt_entry.flag cannot be Upper (ie must be Lower or Exact).
            return tt_entry.score;
    }
    // Internal iterative reduction
    else
        depth -= depth > 3;

    i32 static_eval = stack[ply].score = in_check ? -inf : eval(pos);
    const i32 improving = ply > 1 && static_eval > stack[ply - 2].score;

    // If static_eval > tt_entry.score, tt_entry.flag cannot be Lower (ie must be Upper or Exact).
    // Otherwise, tt_entry.flag cannot be Upper (ie must be Lower or Exact).
    if (!in_check && tt_entry.key == tt_key && tt_entry.flag != static_eval > tt_entry.score)
        static_eval = tt_entry.score;

    if (in_qsearch && static_eval > alpha) {
        if (static_eval >= beta)
            return beta;
        alpha = static_eval;
    }

    if (ply > 0 && !in_qsearch && !in_check && alpha == beta - 1) {
        // Reverse futility pruning
        if (depth < 7) {
            if (static_eval - 66 * (depth - improving) >= beta)
                return static_eval;

            in_qsearch = static_eval + 256 * depth < alpha;
        }

        // Null move pruning
        if (depth > 2 && static_eval >= beta && do_null && pos.colour[0] & ~(pos.pieces[Pawn] | pos.pieces[King])) {
            Position npos = pos;
            flip(npos);
            npos.ep = 0;
            if (-alphabeta(npos,
                           -beta,
                           -alpha,
                           depth - 4 - depth / 6 - min((static_eval - beta) / 200, 3),
                           ply + 1,
                           // minify enable filter delete
                           nodes,
                           // minify disable filter delete
                           stop_time,
                           stop,
                           stack,
                           hh_table,
                           hash_history,
                           false) >= beta)
                return beta;
        }
    }

    hash_history.emplace_back(tt_key);
    uint16_t tt_flag = Upper;

    i32 num_moves_evaluated = 0;
    i32 num_quiets_evaluated = 0;
    i32 best_score = in_qsearch ? static_eval : -inf;
    auto best_move = tt_move;

    auto &moves = stack[ply].moves;
    auto &move_scores = stack[ply].move_scores;
    auto &quiets_evaluated = stack[ply].quiets_evaluated;
    const i32 num_moves = movegen(pos, moves, in_qsearch);

    for (i32 i = 0; i < num_moves; ++i) {
        // Score moves at the first loop, except if we have a hash move,
        // then we'll use that first and delay sorting one iteration.
        if (i == !(no_move == tt_move))
            for (i32 j = 0; j < num_moves; ++j) {
                const i32 gain = max_material[moves[j].promo] + max_material[piece_on(pos, moves[j].to)];
                move_scores[j] = gain                            ? gain + (1LL << 54)
                                 : moves[j] == stack[ply].killer ? 1LL << 50
                                                                 : hh_table[pos.flipped][moves[j].from][moves[j].to];
            }

        // Find best move remaining
        i32 best_move_index = i;
        for (i32 j = i; j < num_moves; ++j) {
            if (moves[j] == tt_move) {
                best_move_index = j;
                break;
            }
            if (move_scores[j] > move_scores[best_move_index])
                best_move_index = j;
        }

        const Move move = moves[best_move_index];
        moves[best_move_index] = moves[i];
        move_scores[best_move_index] = move_scores[i];

        // Material gain
        const i32 gain = max_material[move.promo] + max_material[piece_on(pos, move.to)];

        // Delta pruning
        if (in_qsearch && !in_check && static_eval + 50 + gain < alpha)
            break;

        // Forward futility pruning
        if (ply > 0 && depth < 8 && !in_qsearch && !in_check && num_moves_evaluated &&
            static_eval + 100 * depth + gain < alpha)
            break;

        Position npos = pos;
        if (!makemove(npos, move))
            continue;

        // minify enable filter delete
        nodes++;
        // minify disable filter delete

        i32 score;
        if (!num_moves_evaluated)
        full_window:
            score = -alphabeta(npos,
                               -beta,
                               -alpha,
                               depth - 1,
                               ply + 1,
                               // minify enable filter delete
                               nodes,
                               // minify disable filter delete
                               stop_time,
                               stop,
                               stack,
                               hh_table,
                               hash_history);
        else {
            // Late move reduction
            i32 reduction = depth > 2 && num_moves_evaluated > 4 && !gain
                                ? num_moves_evaluated / 14 + depth / 17 + (alpha == beta - 1) + !improving +
                                      (hh_table[pos.flipped][move.from][move.to] < 0) -
                                      (hh_table[pos.flipped][move.from][move.to] > 0)
                                : 0;

        zero_window:
            score = -alphabeta(npos,
                               -alpha - 1,
                               -alpha,
                               depth - reduction - 1,
                               ply + 1,
                               // minify enable filter delete
                               nodes,
                               // minify disable filter delete
                               stop_time,
                               stop,
                               stack,
                               hh_table,
                               hash_history);

            if (reduction > 0 && score > alpha) {
                reduction = 0;
                goto zero_window;
            }

            if (score > alpha && score < beta)
                goto full_window;
        }

        // Exit early if out of time
        if (depth > 4 && (stop || now() >= stop_time)) {
            hash_history.pop_back();
            return 0;
        }

        num_moves_evaluated++;
        if (!gain)
            quiets_evaluated[num_quiets_evaluated++] = move;

        if (score > best_score) {
            best_score = score;
            if (score > alpha) {
                best_move = move;
                tt_flag = Exact;
                alpha = score;
                stack[ply].move = move;
                if (score >= beta) {
                    tt_flag = Lower;
                    if (!gain) {
                        hh_table[pos.flipped][move.from][move.to] += depth * depth;
                        for (i32 j = 0; j < num_quiets_evaluated - 1; ++j)
                            hh_table[pos.flipped][quiets_evaluated[j].from][quiets_evaluated[j].to] -= depth * depth;
                        stack[ply].killer = move;
                    }
                    break;
                }
            }
        }
        // Late move pruning based on quiet move count
        if (!in_check && alpha == beta - 1 && num_quiets_evaluated > 3 + depth * depth >> !improving)
            break;
    }
    hash_history.pop_back();

    // Return mate or draw scores if no moves found
    if (best_score == -inf)
        return in_check ? ply - mate_score : 0;

    // Save to TT
    tt_entry = {tt_key, best_move, best_score, in_qsearch ? 0 : depth, tt_flag};

    return best_score;
}

// minify enable filter delete
[[nodiscard]] i32 is_pseudolegal_move(const Position &pos, const Move &move) {
    Move moves[256];
    const i32 num_moves = movegen(pos, moves, false);
    for (i32 i = 0; i < num_moves; ++i)
        if (moves[i] == move)
            return true;
    return false;
}
// minify disable filter delete

// minify enable filter delete
void print_pv(const Position &pos, const Move move, vector<u64> &hash_history) {
    // Check move pseudolegality
    if (!is_pseudolegal_move(pos, move))
        return;

    // Check move legality
    Position npos = pos;
    if (!makemove(npos, move))
        return;

    // Print current move
    cout << " " << move_str(move, pos.flipped);

    // Probe the TT in the resulting position
    const u64 tt_key = get_hash(npos);
    const TT_Entry &tt_entry = transposition_table[tt_key % num_tt_entries];

    // Only continue if the move was valid and comes from a PV search
    if (tt_entry.key != tt_key || tt_entry.move == no_move || tt_entry.flag != 2)
        return;

    // Avoid infinite recursion on a repetition
    for (const u64 old_hash : hash_history)
        if (old_hash == tt_key)
            return;

    hash_history.emplace_back(tt_key);
    print_pv(npos, tt_entry.move, hash_history);
    hash_history.pop_back();
}
// minify disable filter delete

auto iteratively_deepen(Position &pos,
                        vector<u64> &hash_history,
                        // minify enable filter delete
                        i32 thread_id,
                        const i32 bench_depth,
                        u64 &total_nodes,
                        // minify disable filter delete
                        const u64 start_time,
                        const i32 allocated_time,
                        i32 &stop) {
    Stack stack[128] = {};
    int64_t hh_table[2][64][64] = {};
    // minify enable filter delete
    u64 nodes = 0;
    // minify disable filter delete

    i32 score = 0;
    for (i32 i = 1; i < 128; ++i) {
        i32 window = 32 + (score * score >> 14);
        i32 research = 0;
    research:
        const i32 newscore = alphabeta(pos,
                                       score - window,
                                       score + window,
                                       i,
                                       0,
                                       // minify enable filter delete
                                       nodes,
                                       // minify disable filter delete
                                       start_time + allocated_time,
                                       stop,
                                       stack,
                                       hh_table,
                                       hash_history);

        // Hard time limit exceeded
        if (now() >= start_time + allocated_time || stop)
            break;

        // minify enable filter delete
        // The main search thread prints with every iteration normally, or when the target depth has finished when
        // benchmarking
        if (thread_id == 0 &&
            (bench_depth == 0 || i == bench_depth && newscore < score + window && newscore > score - window)) {
            const u64 elapsed = now() - start_time;
            cout << "info";
            cout << " depth " << i;
            cout << " score cp " << newscore;
            if (newscore >= score + window)
                cout << " lowerbound";
            else if (newscore <= score - window)
                cout << " upperbound";
            cout << " time " << elapsed;
            cout << " nodes " << nodes;
            if (elapsed > 0)
                cout << " nps " << nodes * 1000 / elapsed;
            // Not a lowerbound - a fail low won't have a meaningful PV.
            if (newscore > score - window) {
                cout << " pv";
                print_pv(pos, stack[0].move, hash_history);
            }
            cout << "\n";
        }

        // OpenBench compliance
        if (bench_depth > 0 && i >= bench_depth && newscore < score + window && newscore > score - window) {
            total_nodes += nodes;
            break;
        }
        // minify disable filter delete

        if (newscore >= score + window || newscore <= score - window) {
            window <<= ++research;
            score = newscore;
            goto research;
        }

        score = newscore;

        // Early exit after completed ply
        if (!research && now() >= start_time + allocated_time / 10)
            break;
    }
    return stack[0].move;
}

// minify enable filter delete
void set_fen(Position &pos, const string &fen) {
    if (fen == "startpos") {
        pos = Position();
        return;
    }

    // Clear
    pos.colour = {};
    pos.pieces = {};
    pos.castling = {};

    stringstream ss{fen};
    string word;

    ss >> word;
    i32 i = 56;
    for (const auto c : word)
        if (c >= '1' && c <= '8')
            i += c - '1' + 1;
        else if (c == '/')
            i -= 16;
        else {
            const i32 side = c == 'p' || c == 'n' || c == 'b' || c == 'r' || c == 'q' || c == 'k';
            const i32 piece = c == 'p' || c == 'P'   ? Pawn
                              : c == 'n' || c == 'N' ? Knight
                              : c == 'b' || c == 'B' ? Bishop
                              : c == 'r' || c == 'R' ? Rook
                              : c == 'q' || c == 'Q' ? Queen
                                                     : King;
            pos.colour.at(side) ^= 1ULL << i;
            pos.pieces.at(piece) ^= 1ULL << i;
            i++;
        }

    // Side to move
    ss >> word;
    const i32 black_move = word == "b";

    // Castling permissions
    ss >> word;
    for (const auto c : word) {
        pos.castling[0] |= c == 'K';
        pos.castling[1] |= c == 'Q';
        pos.castling[2] |= c == 'k';
        pos.castling[3] |= c == 'q';
    }

    // En passant
    ss >> word;
    if (word != "-") {
        const i32 sq = word[0] - 'a' + 8 * (word[1] - '1');
        pos.ep = 1ULL << sq;
    }

    // Flip the board if necessary
    if (black_move)
        flip(pos);
}
// minify disable filter delete

// minify enable filter delete
[[nodiscard]] u64 perft(const Position &pos, const i32 depth) {
    if (depth == 0)
        return 1;

    u64 nodes = 0;
    Move moves[256];
    const i32 num_moves = movegen(pos, moves, false);

    for (i32 i = 0; i < num_moves; ++i) {
        Position npos = pos;

        // Check move legality
        if (!makemove(npos, moves[i]))
            continue;

        nodes += perft(npos, depth - 1);
    }

    return nodes;
}
// minify disable filter delete

i32 main(
    // minify enable filter delete
    const i32 argc,
    const char **argv
    // minify disable filter delete
) {
    setbuf(stdout, 0);

    // Generate used attack masks
    for (i32 i = 0; i < 64; ++i)
        diag_mask[i] = ray(i, 0, ne) | ray(i, 0, sw);

    mt19937_64 r;
    // pieces from 1-12 multiplied by the square + ep squares + castling rights
    for (u64 &k : keys)
        k = r();

    Position pos;
    vector<u64> hash_history;

    // minify enable filter delete
    // OpenBench compliance
    u64 total_nodes = 0;
    if (argc > 1 && argv[1] == string("bench")) {
        // Initialise the TT
        transposition_table.resize(num_tt_entries);

        const pair<string, int> bench_positions[] = {
            {"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 14},         // Phase 24
            {"r1n2rk1/1pq2pp1/4bn1p/N1bpp3/4P3/3QBP2/PPP1B1PP/2KR3R w - - 0 1", 13},  // Phase 23
            {"r1bq1rk1/pp2p1bp/2p3p1/1P3p2/2QPn3/6P1/PB1NPPBP/R4RK1 b - - 0 1", 14},  // Phase 22
            {"r3r1k1/ppqn1p2/4bppp/8/2PB4/1P1B2b1/P1Q2PPP/R3R1K1 w - - 0 1", 14},     // Phase 21
            {"r4rk1/pp3ppp/2nppq2/8/2BPb3/4P3/PP1N1PPP/R2Q1RK1 b - - 0 1", 13},       // Phase 20
            {"r2q2k1/1p1r4/p4b2/2Bn2p1/4Q3/6PP/PP3P2/R2R2K1 w - - 0 1", 12},          // Phase 19
            {"2bq1rk1/1pn2ppp/3p4/p2Np3/4P1P1/PN2BP2/1PP3QP/2K1b2R w - - 0 1", 15},   // Phase 18
            {"2rr3k/1p3pp1/1Q2b2p/pP2p3/P1B1P3/3P2R1/5P1P/2q3NK w - - 0 1", 15},      // Phase 17
            {"4k2r/3nbp1p/pqp1p3/3p2p1/3P1B2/4PPN1/PP2QP1P/2R3K1 b w k - 0 1", 14},   // Phase 16
            {"6k1/5p2/p3p1p1/2qnQ3/6P1/1r5P/1P2R2K/5R2 w - - 0 1", 13},               // Phase 15
            {"q1b2rk1/2R1Qp1p/p5p1/1p6/1P1Np2P/P3P1P1/5P2/6K1 b - - 0 1", 15},        // Phase 14
            {"6k1/3q2b1/1nNp4/3Pp1pp/4Pp2/5P2/6PP/2RQ2K1 w - - 0 1", 14},             // Phase 13
            {"1r3rk1/p4p2/2p1bp1p/3Pb3/8/1P1BN3/P4PPP/4RRK1 b - - 0 1", 15},          // Phase 12
            {"5b2/5k1p/4bpp1/2p5/4Pq2/3PN2P/2P2PP1/Q5K1 w - - 0 1", 13},              // Phase 11
            {"3k4/2p5/p2qn3/1p1pNQ2/1P6/P6P/6P1/5K2 b - - 0 1", 14},                  // Phase 10
            {"r1b3k1/2p2ppp/1b6/p3N3/5B2/2P5/PP4PP/R2n1K2 b - - 0 1", 14},            // Phase 9
            {"2k5/1pp2Q1p/8/8/5P2/3q4/P5K1/8 b - - 0 1", 17},                         // Phase 8
            {"8/1R4b1/3k4/3npp1p/3p3P/r2P1NP1/4PP2/6K1 w - - 0 1", 16},               // Phase 7
            {"8/2p2p2/2Bb1kp1/R6p/P6P/r5P1/4PPK1/8 b - - 0 1", 13},                   // Phase 6
            {"8/5b2/pr4p1/4k2p/8/P1K3P1/7P/5R2 w - - 0 1", 15},                       // Phase 5
            {"8/7R/8/4p1k1/1r6/3K4/8/8 b - - 0 1", 15},                               // Phase 4
            {"8/8/5kp1/5p1p/4pP1P/6P1/4K3/2R3b1 b - - 0 1", 22},                      // Phase 3
            {"8/8/3bB3/7p/8/pK4kP/8/8 w - - 0 1", 25},                                // Phase 2
            {"8/5k2/P6P/5b2/P7/3p4/3K4/8 b - - 0 1", 18},                             // Phase 1
            {"8/8/p1Pk2pp/3p1p2/p4P2/6P1/5K1P/8 w - - 0 1", 16}                       // Phase 0
        };

        const u64 start_time = now();
        for (const auto &[fen, depth] : bench_positions) {
            i32 stop = false;
            set_fen(pos, fen);
            iteratively_deepen(pos, hash_history, 0, depth, total_nodes, now(), 1 << 30, stop);
        }
        const u64 elapsed = now() - start_time;

        cout << "Bench: ";
        cout << elapsed << " ms ";
        cout << total_nodes << " nodes ";
        cout << total_nodes * 1000 / max(elapsed, static_cast<u64>(1)) << " nps\n";

        return 0;
    }
    // minify disable filter delete

    string word;

    // Wait for "uci"
    cin >> word;

    // Send UCI info
    cout << "id name 4ku\n";
    cout << "id author kz04px\n";
    // minify enable filter delete
    cout << "option name Threads type spin default " << thread_count << " min 1 max 256\n";
    cout << "option name Hash type spin default " << (num_tt_entries >> 15) << " min 1 max 65536\n";
    // minify disable filter delete
    cout << "uciok\n";

    // Initialise the TT
    transposition_table.resize(num_tt_entries);

    while (true) {
        cin >> word;
        if (word == "quit"
            // minify enable filter delete
            || !cin.good()
            // minify disable filter delete
        )
            break;
        else if (word == "ucinewgame")
            memset(transposition_table.data(), 0, sizeof(TT_Entry) * transposition_table.size());
        else if (word == "isready")
            cout << "readyok\n";
        // minify enable filter delete
        else if (word == "setoption") {
            cin >> word;
            cin >> word;
            if (word == "Threads") {
                cin >> word;
                cin >> thread_count;
                thread_count = max(1, min(256, thread_count));
            } else if (word == "Hash") {
                i32 megabytes = 1;
                cin >> word;
                cin >> megabytes;
                num_tt_entries = static_cast<u64>(min(max(megabytes, 1), 65536)) * 1024 * 1024 / sizeof(TT_Entry);
                transposition_table.clear();
                transposition_table.resize(num_tt_entries);
            }
        }
        // minify disable filter delete
        else if (word == "go") {
            i32 time_left;

            // minify enable filter delete
            while (true) {
                cin >> word;
                if (word == (pos.flipped ? "btime" : "wtime")) {
                    cin >> time_left;
                    break;
                }
            }

            goto search_start;
            // minify disable filter delete

            cin >> word >> time_left;
            if (pos.flipped)
                cin >> word >> time_left;

        // minify enable filter delete
        search_start:
            // minify disable filter delete

            const u64 start = now();

            // Lazy SMP
            vector<thread> threads;
            i32 stop = false;
            for (i32 i = 1; i < thread_count; ++i)
                threads.emplace_back([=, &stop]() mutable {
                    iteratively_deepen(pos,
                                       hash_history,
                                       // minify enable filter delete
                                       i,
                                       0,
                                       total_nodes,
                                       // minify disable filter delete
                                       start,
                                       1 << 30,
                                       stop);
                });
            const Move best_move = iteratively_deepen(pos,
                                                      hash_history,
                                                      // minify enable filter delete
                                                      0,
                                                      0,
                                                      total_nodes,
                                                      // minify disable filter delete
                                                      start,
                                                      time_left / 3,
                                                      stop);
            stop = true;
            for (i32 i = 1; i < thread_count; ++i)
                threads[i - 1].join();
            cout << "bestmove " << move_str(best_move, pos.flipped) << "\n";
        } else if (word == "position") {
            // Set to startpos
            pos = Position();
            hash_history.clear();

            // minify enable filter delete
            string fen;
            i32 fen_size = 0;

            // Try collect FEN string
            while (fen_size < 6 && cin >> word) {
                if (word == "moves" || word == "startpos")
                    break;
                else if (word != "fen") {
                    if (fen.empty())
                        fen = word;
                    else
                        fen += " " + word;
                    fen_size++;
                }
            }

            if (!fen.empty())
                set_fen(pos, fen);
            // minify disable filter delete
        }
        // minify enable filter delete
        else if (word == "perft") {
            i32 depth = 0;
            cin >> depth;
            const auto t0 = std::chrono::steady_clock::now();
            const auto nodes = perft(pos, depth);
            const auto t1 = std::chrono::steady_clock::now();
            const auto dt = std::chrono::duration_cast<std::chrono::milliseconds>(t1 - t0);
            const auto nps = dt.count() ? 1000 * nodes / dt.count() : 0;

            std::cout << "info";
            std::cout << " depth " << depth;
            std::cout << " nodes " << nodes;
            std::cout << " time " << dt.count();
            if (nps > 0)
                std::cout << " nps " << nps;
            std::cout << std::endl;
            std::cout << "nodes " << nodes << "\n";
        }
        // minify disable filter delete
        else {
            Move moves[256];
            const i32 num_moves = movegen(pos, moves, false);
            for (i32 i = 0; i < num_moves; ++i) {
                if (word == move_str(moves[i], pos.flipped)) {
                    if (piece_on(pos, moves[i].to) == None && piece_on(pos, moves[i].from))
                        hash_history.emplace_back(get_hash(pos));
                    else
                        hash_history.clear();

                    makemove(pos, moves[i]);
                    break;
                }
            }
        }
    }
}
