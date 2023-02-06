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

#define MATE_SCORE (1 << 15)
#define INF (1 << 16)

using namespace std;

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

[[nodiscard]] int64_t now() {
    timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec * 1000 + t.tv_nsec / 1000000;
}

using u64 = uint64_t;

struct [[nodiscard]] Position {
    array<int, 4> castling = {true, true, true, true};
    array<u64, 2> colour = {0xFFFFULL, 0xFFFF000000000000ULL};
    array<u64, 6> pieces = {0xFF00000000FF00ULL,
                            0x4200000000000042ULL,
                            0x2400000000000024ULL,
                            0x8100000000000081ULL,
                            0x800000000000008ULL,
                            0x1000000000000010ULL};
    u64 ep = 0x0ULL;
    int flipped = false;
};

struct Move {
    int from = 0;
    int to = 0;
    int promo = 0;
};

const Move no_move{};

struct [[nodiscard]] Stack {
    Move moves[256];
    Move quiets_evaluated[256];
    int64_t move_scores[256];
    Move move;
    Move killer;
    int score;
};

struct [[nodiscard]] TT_Entry {
    u64 key;
    Move move;
    int score;
    int depth;
    uint16_t flag;
};

u64 keys[848];

// Engine options
auto num_tt_entries = 64ULL << 15;  // The first value is the size in megabytes
auto thread_count = 1;

vector<TT_Entry> transposition_table;

[[nodiscard]] u64 flip(const u64 bb) {
    return __builtin_bswap64(bb);
}

[[nodiscard]] auto lsb(const u64 bb) {
    return __builtin_ctzll(bb);
}

[[nodiscard]] auto count(const u64 bb) {
    return __builtin_popcountll(bb);
}

[[nodiscard]] auto east(const u64 bb) {
    return (bb << 1) & ~0x0101010101010101ULL;
}

[[nodiscard]] auto west(const u64 bb) {
    return (bb >> 1) & ~0x8080808080808080ULL;
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

[[nodiscard]] auto operator==(const Move &lhs, const Move &rhs) {
    return !memcmp(&rhs, &lhs, sizeof(Move));
}

[[nodiscard]] auto move_str(const Move &move, const int flip) {
    string str;
    str += 'a' + (move.from % 8);
    str += '1' + (flip ? (7 - move.from / 8) : (move.from / 8));
    str += 'a' + (move.to % 8);
    str += '1' + (flip ? (7 - move.to / 8) : (move.to / 8));
    if (move.promo != None) {
        str += "\0nbrq\0\0"[move.promo];
    }
    return str;
}

[[nodiscard]] int piece_on(const Position &pos, const int sq) {
    const u64 bb = 1ULL << sq;
    for (int i = 0; i < 6; ++i) {
        if (pos.pieces[i] & bb) {
            return i;
        }
    }
    return None;
}

void flip(Position &pos) {
    pos.colour[0] = flip(pos.colour[0]);
    pos.colour[1] = flip(pos.colour[1]);
    for (int i = 0; i < 6; ++i) {
        pos.pieces[i] = flip(pos.pieces[i]);
    }
    pos.ep = flip(pos.ep);
    swap(pos.colour[0], pos.colour[1]);
    swap(pos.castling[0], pos.castling[2]);
    swap(pos.castling[1], pos.castling[3]);
    pos.flipped = !pos.flipped;
}

template <typename F>
[[nodiscard]] auto ray(const int sq, const u64 blockers, F f) {
    u64 mask = f(1ULL << sq);
    mask |= f(mask & ~blockers);
    mask |= f(mask & ~blockers);
    mask |= f(mask & ~blockers);
    mask |= f(mask & ~blockers);
    mask |= f(mask & ~blockers);
    mask |= f(mask & ~blockers);
    return mask;
}

[[nodiscard]] u64 knight(const int sq, const u64) {
    const u64 bb = 1ULL << sq;
    return (((bb << 15) | (bb >> 17)) & 0x7F7F7F7F7F7F7F7FULL) | (((bb << 17) | (bb >> 15)) & 0xFEFEFEFEFEFEFEFEULL) |
           (((bb << 10) | (bb >> 6)) & 0xFCFCFCFCFCFCFCFCULL) | (((bb << 6) | (bb >> 10)) & 0x3F3F3F3F3F3F3F3FULL);
}

[[nodiscard]] auto bishop(const int sq, const u64 blockers) {
    return ray(sq, blockers, nw) | ray(sq, blockers, ne) | ray(sq, blockers, sw) | ray(sq, blockers, se);
}

[[nodiscard]] auto rook(const int sq, const u64 blockers) {
    return ray(sq, blockers, north) | ray(sq, blockers, east) | ray(sq, blockers, south) | ray(sq, blockers, west);
}

[[nodiscard]] u64 king(const int sq, const u64) {
    const u64 bb = 1ULL << sq;
    return (bb << 8) | (bb >> 8) | (((bb >> 1) | (bb >> 9) | (bb << 7)) & 0x7F7F7F7F7F7F7F7FULL) |
           (((bb << 1) | (bb << 9) | (bb >> 7)) & 0xFEFEFEFEFEFEFEFEULL);
}

[[nodiscard]] auto attacked(const Position &pos, const int sq, const int them = true) {
    const u64 bb = 1ULL << sq;
    const u64 kt = pos.colour[them] & pos.pieces[Knight];
    const u64 BQ = pos.pieces[Bishop] | pos.pieces[Queen];
    const u64 RQ = pos.pieces[Rook] | pos.pieces[Queen];
    const u64 pawns = pos.colour[them] & pos.pieces[Pawn];
    const u64 pawn_attacks = them ? sw(pawns) | se(pawns) : nw(pawns) | ne(pawns);
    return (pawn_attacks & bb) | (kt & knight(sq, 0)) |
           (bishop(sq, pos.colour[0] | pos.colour[1]) & pos.colour[them] & BQ) |
           (rook(sq, pos.colour[0] | pos.colour[1]) & pos.colour[them] & RQ) |
           (king(sq, 0) & pos.colour[them] & pos.pieces[King]);
}

auto makemove(Position &pos, const Move &move) {
    const int piece = piece_on(pos, move.from);
    const int captured = piece_on(pos, move.to);
    const u64 to = 1ULL << move.to;
    const u64 from = 1ULL << move.from;

    // Move the piece
    pos.colour[0] ^= from | to;
    pos.pieces[piece] ^= from | to;

    // En passant
    if (piece == Pawn && to == pos.ep) {
        pos.colour[1] ^= to >> 8;
        pos.pieces[Pawn] ^= to >> 8;
    }

    pos.ep = 0;

    // Pawn double move
    if (piece == Pawn && move.to - move.from == 16) {
        pos.ep = to >> 8;
    }

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
    pos.castling[0] &= !((from | to) & 0x90ULL);
    pos.castling[1] &= !((from | to) & 0x11ULL);
    pos.castling[2] &= !((from | to) & 0x9000000000000000ULL);
    pos.castling[3] &= !((from | to) & 0x1100000000000000ULL);

    flip(pos);

    // Return move legality
    return !attacked(pos, lsb(pos.colour[1] & pos.pieces[King]), false);
}

void generate_pawn_moves(Move *const movelist, int &num_moves, u64 to_mask, const int offset) {
    while (to_mask) {
        const int to = lsb(to_mask);
        to_mask &= to_mask - 1;
        if (to >= 56) {
            movelist[num_moves++] = Move{to + offset, to, Queen};
            movelist[num_moves++] = Move{to + offset, to, Rook};
            movelist[num_moves++] = Move{to + offset, to, Bishop};
            movelist[num_moves++] = Move{to + offset, to, Knight};
        } else {
            movelist[num_moves++] = Move{to + offset, to, None};
        }
    }
}

template <typename F>
void generate_piece_moves(Move *const movelist,
                          int &num_moves,
                          const Position &pos,
                          const int piece,
                          const u64 to_mask,
                          F f) {
    u64 copy = pos.colour[0] & pos.pieces[piece];
    while (copy) {
        const int fr = lsb(copy);
        copy &= copy - 1;
        u64 moves = f(fr, pos.colour[0] | pos.colour[1]) & to_mask;
        while (moves) {
            const int to = lsb(moves);
            moves &= moves - 1;
            movelist[num_moves++] = Move{fr, to, None};
        }
    }
}

[[nodiscard]] auto movegen(const Position &pos, Move *const movelist, const bool only_captures) {
    int num_moves = 0;
    const u64 all = pos.colour[0] | pos.colour[1];
    const u64 to_mask = only_captures ? pos.colour[1] : ~pos.colour[0];
    const u64 pawns = pos.colour[0] & pos.pieces[Pawn];
    generate_pawn_moves(movelist, num_moves, north(pawns) & ~all & (only_captures ? 0xFF00000000000000ULL : ~0ULL), -8);
    if (!only_captures) {
        generate_pawn_moves(movelist, num_moves, north(north(pawns & 0xFF00ULL) & ~all) & ~all, -16);
    }
    generate_pawn_moves(movelist, num_moves, nw(pawns) & (pos.colour[1] | pos.ep), -7);
    generate_pawn_moves(movelist, num_moves, ne(pawns) & (pos.colour[1] | pos.ep), -9);
    generate_piece_moves(movelist, num_moves, pos, Knight, to_mask, knight);
    generate_piece_moves(movelist, num_moves, pos, Bishop, to_mask, bishop);
    generate_piece_moves(movelist, num_moves, pos, Rook, to_mask, rook);
    generate_piece_moves(movelist, num_moves, pos, Queen, to_mask, rook);
    generate_piece_moves(movelist, num_moves, pos, Queen, to_mask, bishop);
    generate_piece_moves(movelist, num_moves, pos, King, to_mask, king);
    if (!only_captures && pos.castling[0] && !(all & 0x60ULL) && !attacked(pos, 4) && !attacked(pos, 5)) {
        movelist[num_moves++] = Move{4, 6, None};
    }
    if (!only_captures && pos.castling[1] && !(all & 0xEULL) && !attacked(pos, 4) && !attacked(pos, 3)) {
        movelist[num_moves++] = Move{4, 2, None};
    }
    return num_moves;
}

[[nodiscard]] int S(const int mg, const int eg) {
    return (eg << 16) + mg;
}

const int phases[] = {0, 1, 1, 2, 4, 0};
const int max_material[] = {116, 352, 378, 662, 1227, 0, 0};
const int material[] = {S(116, 109), S(352, 348), S(378, 365), S(472, 662), S(967, 1227), 0};
const int pst_rank[][8] = {
    {0, S(-9, 4), S(-10, 2), S(-6, 2), S(-2, 3), S(6, 4), S(3, -4), 0},
    {S(-12, -5), S(-6, 0), S(-1, 4), S(4, 8), S(12, 9), S(20, 5), S(11, 2), S(-10, -1)},
    {S(-10, -3), S(-3, 1), S(1, 3), S(3, 5), S(5, 6), S(14, 2), S(4, 3), S(-13, 2)},
    {S(-1, -9), S(-4, -11), S(-5, -9), S(-5, -5), S(1, -2), S(9, -2), S(12, -1), S(5, 0)},
    {S(-8, -20), S(-4, -17), S(-4, -10), S(-6, 1), S(-2, 6), S(6, 3), S(-7, 11), S(-1, 3)},
    {S(-1, -8), S(5, -2), S(-9, 2), S(-11, 5), S(-10, 8), S(-2, 8), S(15, 3), S(-6, -1)},
};
const int pst_file[][4] = {{S(2, -5), S(9, -5), S(6, -5), S(7, -7)},
                           {S(-10, -9), S(-4, -5), S(2, -1), S(3, 1)},
                           {S(-4, -5), S(2, -3), S(1, -1), S(2, 0)},
                           {S(-3, 0), S(-2, 1), S(-1, 2), S(0, 1)},
                           {S(-9, 0), S(-6, 1), S(-4, 5), S(-3, 5)},
                           {S(-1, -3), S(1, 1), S(-10, 4), S(1, 1)}};
const int pawn_protection[] = {S(20, 15), S(9, 12), S(2, 5), S(4, 7), S(-6, 19), S(-48, 27)};
const int passers[] = {S(-31, 17), S(-19, 8), S(3, 20), S(19, 52), S(48, 119), S(132, 209)};
const int pawn_doubled = S(-29, -25);
const int pawn_passed_blocked[] = {S(16, -16), S(-35, -4), S(-55, -11), S(5, -35), S(6, -65), S(28, -75)};
const int pawn_passed_king_distance[] = {S(3, -6), S(-6, 9)};
const int bishop_pair = S(27, 68);
const int rook_open = S(61, 7);
const int rook_semi_open = S(27, 20);
const int king_shield[] = {S(40, -5), S(26, -7), S(-85, 18)};
const int pawn_attacked[] = {S(-64, -14), S(-155, -142)};

[[nodiscard]] int eval(Position &pos) {
    // Include side to move bonus
    int score = S(28, 10);
    int phase = 0;

    for (int c = 0; c < 2; ++c) {
        // our pawns, their pawns
        const u64 pawns[] = {pos.colour[0] & pos.pieces[Pawn], pos.colour[1] & pos.pieces[Pawn]};
        const u64 protected_by_pawns = nw(pawns[0]) | ne(pawns[0]);
        const u64 attacked_by_pawns = se(pawns[1]) | sw(pawns[1]);
        const int kings[] = {lsb(pos.colour[0] & pos.pieces[King]), lsb(pos.colour[1] & pos.pieces[King])};

        // Bishop pair
        if (count(pos.colour[0] & pos.pieces[Bishop]) == 2) {
            score += bishop_pair;
        }

        // For each piece type
        for (int p = 0; p < 6; ++p) {
            auto copy = pos.colour[0] & pos.pieces[p];
            while (copy) {
                const int sq = lsb(copy);
                copy &= copy - 1;

                // Material
                phase += phases[p];
                score += material[p];

                const int rank = sq / 8;
                const int file = sq % 8;

                // Split quantized PSTs
                score += pst_rank[p][rank] * 4;
                score += pst_file[p][min(file, 7 - file)] * 4;

                // Pawn protection
                const u64 piece_bb = 1ULL << sq;
                if (piece_bb & protected_by_pawns) {
                    score += pawn_protection[p];
                }
                if (~pawns[0] & piece_bb & attacked_by_pawns) {
                    // If we're to move, we'll just lose some options and our tempo.
                    // If we're not to move, we lose a piece?
                    score += pawn_attacked[c];
                }

                if (p == Pawn) {
                    // Passed pawns
                    u64 blockers = 0x101010101010101ULL << sq;
                    blockers |= nw(blockers) | ne(blockers);
                    if (!(blockers & pawns[1])) {
                        score += passers[rank - 1];

                        // Blocked passed pawns
                        if (north(piece_bb) & pos.colour[1]) {
                            score += pawn_passed_blocked[rank - 1];
                        }

                        // King defense/attack
                        // king distance to square in front of passer
                        for (int i = 0; i < 2; ++i) {
                            score += pawn_passed_king_distance[i] * (rank - 1) *
                                     max(abs((kings[i] / 8) - (rank + 1)), abs((kings[i] % 8) - file));
                        }
                    }

                    // Doubled pawns
                    if ((north(piece_bb) | north(north(piece_bb))) & pawns[0]) {
                        score += pawn_doubled;
                    }
                } else if (p == Rook) {
                    // Rook on open or semi-open files
                    const u64 file_bb = 0x101010101010101ULL << file;
                    if (!(file_bb & pawns[0])) {
                        if (!(file_bb & pawns[1])) {
                            score += rook_open;
                        } else {
                            score += rook_semi_open;
                        }
                    }
                } else if (p == King && piece_bb & 0xE7) {
                    const u64 shield = file < 3 ? 0x700 : 0xE000;
                    score += count(shield & pawns[0]) * king_shield[0];
                    score += count(north(shield) & pawns[0]) * king_shield[1];

                    // C3D7 = Reasonable king squares
                    score += !(piece_bb & 0xC3D7) * king_shield[2];
                }
            }
        }

        flip(pos);

        score = -score;
    }

    // Tapered eval
    return ((short)score * phase + ((score + 0x8000) >> 16) * (24 - phase)) / 24;
}

[[nodiscard]] auto get_hash(const Position &pos) {
    u64 hash = pos.flipped;

    // Pieces
    for (int p = Pawn; p < None; p++) {
        u64 copy = pos.pieces[p] & pos.colour[0];
        while (copy) {
            const int sq = lsb(copy);
            copy &= copy - 1;
            hash ^= keys[p * 64 + sq];
        }
        copy = pos.pieces[p] & pos.colour[1];
        while (copy) {
            const int sq = lsb(copy);
            copy &= copy - 1;
            hash ^= keys[(p + 6) * 64 + sq];
        }
    }

    // En passant square
    if (pos.ep) {
        hash ^= keys[768 + lsb(pos.ep)];
    }

    // Castling permissions
    hash ^= keys[832 + (pos.castling[0] | pos.castling[1] << 1 | pos.castling[2] << 2 | pos.castling[3] << 3)];

    return hash;
}

int alphabeta(Position &pos,
              int alpha,
              const int beta,
              int depth,
              const int ply,
              // minify enable filter delete
              int64_t &nodes,
              // minify disable filter delete
              const int64_t stop_time,
              int &stop,
              Stack *const stack,
              int64_t (&hh_table)[2][64][64],
              vector<u64> &hash_history,
              const int do_null = true) {
    const int static_eval = eval(pos);

    // Don't overflow the stack
    if (ply > 127) {
        return static_eval;
    }

    stack[ply].score = static_eval;
    const auto improving = ply > 1 && static_eval > stack[ply - 2].score;

    // Check extensions
    const auto in_check = attacked(pos, lsb(pos.colour[0] & pos.pieces[King]));
    depth += !!in_check;

    const int in_qsearch = depth <= 0;
    if (in_qsearch && static_eval > alpha) {
        if (static_eval >= beta) {
            return beta;
        }
        alpha = static_eval;
    }

    const u64 tt_key = get_hash(pos);

    if (ply > 0 && !in_qsearch) {
        // Repetition detection
        for (const auto old_hash : hash_history) {
            if (old_hash == tt_key) {
                return 0;
            }
        }

        if (!in_check && alpha == beta - 1) {
            // Reverse futility pruning
            if (depth < 7) {
                const int margins[] = {0, 50, 100, 200, 300, 500, 800};
                if (static_eval - margins[depth - improving] >= beta) {
                    return beta;
                }
            }

            // Null move pruning
            if (depth > 2 && static_eval >= beta && do_null && pos.colour[0] & ~(pos.pieces[Pawn] | pos.pieces[King])) {
                auto npos = pos;
                flip(npos);
                npos.ep = 0;
                if (-alphabeta(npos,
                               -beta,
                               -beta + 1,
                               depth - 4 - depth / 6,
                               ply + 1,
                               // minify enable filter delete
                               nodes,
                               // minify disable filter delete
                               stop_time,
                               stop,
                               stack,
                               hh_table,
                               hash_history,
                               false) >= beta) {
                    return beta;
                }
            }
        }
    }

    // TT Probing
    TT_Entry &tt_entry = transposition_table[tt_key % num_tt_entries];
    Move tt_move{};
    if (tt_entry.key == tt_key) {
        tt_move = tt_entry.move;
        if (ply > 0 && tt_entry.depth >= depth) {
            if (tt_entry.flag == 0 && tt_entry.score <= alpha) {
                return tt_entry.score;
            }
            if (tt_entry.flag == 1 && tt_entry.score >= beta) {
                return tt_entry.score;
            }
            if (tt_entry.flag == 2) {
                return tt_entry.score;
            }
        }
    }
    // Internal iterative reduction
    else if (depth > 3) {
        depth--;
    }

    // Exit early if out of time
    if (depth > 3 && (stop || now() >= stop_time)) {
        return 0;
    }

    hash_history.emplace_back(tt_key);
    uint16_t tt_flag = 0;  // Alpha flag

    int num_moves_evaluated = 0;
    int num_quiets_evaluated = 0;
    int best_score = -INF;
    Move best_move{};

    auto &moves = stack[ply].moves;
    auto &move_scores = stack[ply].move_scores;
    const int num_moves = movegen(pos, moves, in_qsearch);

    for (int i = 0; i < num_moves; ++i) {
        // Score moves at the first loop, except if we have a hash move,
        // then we'll use that first and delay sorting one iteration.
        if (i == !(no_move == tt_move)) {
            for (int j = 0; j < num_moves; ++j) {
                const int capture = piece_on(pos, moves[j].to);
                if (capture != None) {
                    move_scores[j] = (capture + 1) * (1LL << 54);
                } else if (moves[j] == stack[ply].killer) {
                    move_scores[j] = 1LL << 50;
                } else {
                    move_scores[j] = hh_table[pos.flipped][moves[j].from][moves[j].to];
                }
            }
        }

        // Find best move remaining
        int best_move_index = i;
        if (i == 0 && !(no_move == tt_move)) {
            for (int j = i; j < num_moves; ++j) {
                if (moves[j] == tt_move) {
                    best_move_index = j;
                    break;
                }
            }
        } else {
            for (int j = i; j < num_moves; ++j) {
                if (move_scores[j] > move_scores[best_move_index]) {
                    best_move_index = j;
                }
            }
        }

        const auto move = moves[best_move_index];
        const auto best_move_score = move_scores[best_move_index];

        moves[best_move_index] = moves[i];
        move_scores[best_move_index] = move_scores[i];

        // Delta pruning
        if (in_qsearch && !in_check && static_eval + 50 + max_material[piece_on(pos, move.to)] < alpha) {
            best_score = alpha;
            break;
        }

        // Forward futility pruning
        if (!in_qsearch && !in_check && !(move == tt_move) &&
            static_eval + 150 * depth + max_material[piece_on(pos, move.to)] < alpha) {
            best_score = alpha;
            break;
        }

        auto npos = pos;
        if (!makemove(npos, move)) {
            continue;
        }

        // minify enable filter delete
        nodes++;
        // minify disable filter delete

        int score;
        if (!num_moves_evaluated) {
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
        } else {
            // Late move reduction
            int reduction = depth > 2 && num_moves_evaluated > 4 && piece_on(pos, move.to) == None
                                ? 1 + num_moves_evaluated / 14 + depth / 17 + (alpha == beta - 1) - improving +
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

            if (score > alpha && score < beta) {
                goto full_window;
            }
        }

        // Exit early if out of time
        if (depth > 3 && (stop || now() >= stop_time)) {
            hash_history.pop_back();
            return 0;
        }

        num_moves_evaluated++;
        if (piece_on(pos, move.to) == None) {
            stack[ply].quiets_evaluated[num_quiets_evaluated] = move;
            num_quiets_evaluated++;
        }

        if (score > best_score) {
            best_score = score;
            best_move = move;
            if (score > alpha) {
                tt_flag = 2;  // Exact flag
                alpha = score;
                stack[ply].move = move;
            }
        } else if (!in_qsearch && !in_check && alpha == beta - 1 && depth <= 3 &&
                   num_moves_evaluated >= (depth * 3) + 2 && static_eval < alpha - (50 * depth) &&
                   best_move_score < (1LL << 50)) {
            best_score = alpha;
            break;
        }

        if (alpha >= beta) {
            tt_flag = 1;  // Beta flag
            if (piece_on(pos, move.to) == None) {
                hh_table[pos.flipped][move.from][move.to] += depth * depth;
                for (int j = 0; j < num_quiets_evaluated - 1; ++j) {
                    hh_table[pos.flipped][stack[ply].quiets_evaluated[j].from][stack[ply].quiets_evaluated[j].to] -=
                        depth * depth;
                }
                stack[ply].killer = move;
            }
            break;
        }

        // Late move pruning based on quiet move count
        if (!in_check && alpha == beta - 1 && num_quiets_evaluated > 3 + 2 * depth * depth) {
            break;
        }
    }
    hash_history.pop_back();

    // Return mate or draw scores if no moves found
    if (best_score == -INF) {
        return in_qsearch ? alpha : in_check ? ply - MATE_SCORE : 0;
    }

    // Save to TT
    if (tt_entry.key != tt_key || depth >= tt_entry.depth || tt_flag > tt_entry.flag) {
        tt_entry =
            TT_Entry{tt_key, best_move == no_move ? tt_move : best_move, best_score, in_qsearch ? 0 : depth, tt_flag};
    }

    return alpha;
}

// minify enable filter delete
[[nodiscard]] bool is_pseudolegal_move(const Position &pos, const Move &move) {
    Move moves[256];
    const int num_moves = movegen(pos, moves, false);
    for (int i = 0; i < num_moves; ++i) {
        if (moves[i] == move) {
            return true;
        }
    }
    return false;
}
// minify disable filter delete

// minify enable filter delete
void print_pv(const Position &pos, const Move move, vector<u64> &hash_history) {
    // Check move pseudolegality
    if (!is_pseudolegal_move(pos, move)) {
        return;
    }

    // Check move legality
    auto npos = pos;
    if (!makemove(npos, move)) {
        return;
    }

    // Print current move
    cout << " " << move_str(move, pos.flipped);

    // Probe the TT in the resulting position
    const u64 tt_key = get_hash(npos);
    const TT_Entry &tt_entry = transposition_table[tt_key % num_tt_entries];

    // Only continue if the move was valid and comes from a PV search
    if (tt_entry.key != tt_key || tt_entry.move == no_move || tt_entry.flag != 2) {
        return;
    }

    // Avoid infinite recursion on a repetition
    for (const auto old_hash : hash_history) {
        if (old_hash == tt_key) {
            return;
        }
    }

    hash_history.emplace_back(tt_key);
    print_pv(npos, tt_entry.move, hash_history);
    hash_history.pop_back();
}
// minify disable filter delete

auto iteratively_deepen(Position &pos,
                        vector<u64> &hash_history,
                        // minify enable filter delete
                        int thread_id,
                        const bool is_bench,
                        // minify disable filter delete
                        const int64_t start_time,
                        const int allocated_time,
                        int &stop) {
    Stack stack[128] = {};
    int64_t hh_table[2][64][64] = {};
    // minify enable filter delete
    int64_t nodes = 0;
    // minify disable filter delete

    int score = 0;
    for (int i = 1; i < 128; ++i) {
        auto window = 40;
        auto research = 0;
    research:
        const auto newscore = alphabeta(pos,
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
        if (now() >= start_time + allocated_time || stop) {
            break;
        }

        // minify enable filter delete
        if (thread_id == 0) {
            const auto elapsed = now() - start_time;

            cout << "info";
            cout << " depth " << i;
            cout << " score cp " << newscore;
            if (newscore >= score + window) {
                cout << " lowerbound";
            } else if (newscore <= score - window) {
                cout << " upperbound";
            }
            cout << " time " << elapsed;
            cout << " nodes " << nodes;
            if (elapsed > 0) {
                cout << " nps " << nodes * 1000 / elapsed;
            }
            // Not a lowerbound - a fail low won't have a meaningful PV.
            if (newscore > score - window) {
                cout << " pv";
                print_pv(pos, stack[0].move, hash_history);
            }
            cout << "\n";

            // OpenBench compliance
            if (is_bench && i >= 14) {
                cout << "Bench: ";
                cout << elapsed << " ms ";
                cout << nodes << " nodes ";
                cout << nodes * 1000 / max(elapsed, static_cast<int64_t>(1)) << " nps\n";
                break;
            }
        }
        // minify disable filter delete

        if (newscore >= score + window || newscore <= score - window) {
            window <<= ++research;
            score = newscore;
            goto research;
        }

        score = newscore;

        // Early exit after completed ply
        if (!research && now() >= start_time + allocated_time / 10) {
            break;
        }
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
    int i = 56;
    for (const auto c : word) {
        if (c >= '1' && c <= '8') {
            i += c - '1' + 1;
        } else if (c == '/') {
            i -= 16;
        } else {
            const int side = c == 'p' || c == 'n' || c == 'b' || c == 'r' || c == 'q' || c == 'k';
            const int piece = (c == 'p' || c == 'P')   ? Pawn
                              : (c == 'n' || c == 'N') ? Knight
                              : (c == 'b' || c == 'B') ? Bishop
                              : (c == 'r' || c == 'R') ? Rook
                              : (c == 'q' || c == 'Q') ? Queen
                                                       : King;
            pos.colour.at(side) ^= 1ULL << i;
            pos.pieces.at(piece) ^= 1ULL << i;
            i++;
        }
    }

    // Side to move
    ss >> word;
    const bool black_move = word == "b";

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
        const int sq = word[0] - 'a' + 8 * (word[1] - '1');
        pos.ep = 1ULL << sq;
    }

    // Flip the board if necessary
    if (black_move) {
        flip(pos);
    }
}
// minify disable filter delete

// minify enable filter delete
[[nodiscard]] auto perft(const Position &pos, const int depth) -> u64 {
    if (depth == 0) {
        return 1;
    }

    u64 nodes = 0;
    Move moves[256];
    const int num_moves = movegen(pos, moves, false);

    for (int i = 0; i < num_moves; ++i) {
        auto npos = pos;

        // Check move legality
        if (!makemove(npos, moves[i])) {
            continue;
        }

        nodes += perft(npos, depth - 1);
    }

    return nodes;
}
// minify disable filter delete

int main(
    // minify enable filter delete
    const int argc,
    const char **argv
    // minify disable filter delete
) {
    setbuf(stdout, 0);

    mt19937_64 r;
    // pieces from 1-12 multiplied by the square + ep squares + castling rights
    for (auto &k : keys) {
        k = r();
    }

    Position pos;
    vector<u64> hash_history;

    // minify enable filter delete
    // OpenBench compliance
    if (argc > 1 && argv[1] == string("bench")) {
        // Initialise the TT
        transposition_table.resize(num_tt_entries);

        int stop = false;
        iteratively_deepen(pos, hash_history, 0, true, now(), 1 << 30, stop);

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
        ) {
            break;
        } else if (word == "ucinewgame") {
            memset(transposition_table.data(), 0, sizeof(TT_Entry) * transposition_table.size());
        } else if (word == "isready") {
            cout << "readyok\n";
        }
        // minify enable filter delete
        else if (word == "setoption") {
            cin >> word;
            cin >> word;
            if (word == "Threads") {
                cin >> word;
                cin >> thread_count;
                thread_count = max(1, min(256, thread_count));
            } else if (word == "Hash") {
                cin >> word;
                cin >> num_tt_entries;
                num_tt_entries = min(max(num_tt_entries, 1ULL), 65536ULL) * 1024 * 1024 / sizeof(TT_Entry);
                transposition_table.clear();
                transposition_table.resize(num_tt_entries);
            }
        }
        // minify disable filter delete
        else if (word == "go") {
            int wtime;
            int btime;

            // minify enable filter delete
            int found = 0;
            while (found < 2) {
                cin >> word;
                if (word == "wtime") {
                    cin >> wtime;
                    found++;
                } else if (word == "btime") {
                    cin >> btime;
                    found++;
                }
            }

            goto search_start;
            // minify disable filter delete

            cin >> word >> wtime >> word >> btime;

        // minify enable filter delete
        search_start:
            // minify disable filter delete

            const auto start = now();
            const auto allocated_time = (pos.flipped ? btime : wtime) / 3;

            // Lazy SMP
            vector<thread> threads;
            int stop = false;
            for (int i = 1; i < thread_count; ++i) {
                threads.emplace_back([=, &stop]() mutable {
                    iteratively_deepen(pos,
                                       hash_history,
                                       // minify enable filter delete
                                       i,
                                       false,
                                       // minify disable filter delete
                                       start,
                                       1 << 30,
                                       stop);
                });
            }
            const auto best_move = iteratively_deepen(pos,
                                                      hash_history,
                                                      // minify enable filter delete
                                                      0,
                                                      false,
                                                      // minify disable filter delete
                                                      start,
                                                      allocated_time,
                                                      stop);
            stop = true;
            for (int i = 1; i < thread_count; ++i) {
                threads[i - 1].join();
            }

            cout << "bestmove " << move_str(best_move, pos.flipped) << "\n";
        } else if (word == "position") {
            // Set to startpos
            pos = Position();
            hash_history.clear();

            // minify enable filter delete
            string fen;
            int fen_size = 0;

            // Try collect FEN string
            while (fen_size < 6 && cin >> word) {
                if (word == "moves" || word == "startpos") {
                    break;
                } else if (word != "fen") {
                    if (fen.empty()) {
                        fen = word;
                    } else {
                        fen += " " + word;
                    }
                    fen_size++;
                }
            }

            if (!fen.empty()) {
                set_fen(pos, fen);
            }
            // minify disable filter delete
        }
        // minify enable filter delete
        else if (word == "perft") {
            int depth = 0;
            cin >> depth;
            const auto t0 = std::chrono::steady_clock::now();
            const auto nodes = perft(pos, depth);
            const auto t1 = std::chrono::steady_clock::now();
            const auto dt = std::chrono::duration_cast<std::chrono::milliseconds>(t1 - t0);
            const auto nps = dt.count() ? (1000 * nodes) / dt.count() : 0;

            std::cout << "info";
            std::cout << " depth " << depth;
            std::cout << " nodes " << nodes;
            std::cout << " time " << dt.count();
            if (nps > 0) {
                std::cout << " nps " << nps;
            }
            std::cout << std::endl;
            std::cout << "nodes " << nodes << "\n";
        }
        // minify disable filter delete
        else {
            Move moves[256];
            const int num_moves = movegen(pos, moves, false);
            for (int i = 0; i < num_moves; ++i) {
                if (word == move_str(moves[i], pos.flipped)) {
                    if (piece_on(pos, moves[i].to) != None || piece_on(pos, moves[i].from) == Pawn) {
                        hash_history.clear();
                    } else {
                        hash_history.emplace_back(get_hash(pos));
                    }

                    makemove(pos, moves[i]);
                    break;
                }
            }
        }
    }
}
