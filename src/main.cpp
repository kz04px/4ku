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
// minify delete on
#include <sstream>
// minify delete off

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

using BB = uint64_t;

struct [[nodiscard]] Position {
    array<int, 4> castling = {true, true, true, true};
    array<BB, 2> colour = {0xFFFFULL, 0xFFFF000000000000ULL};
    array<BB, 6> pieces = {0xFF00000000FF00ULL,
                           0x4200000000000042ULL,
                           0x2400000000000024ULL,
                           0x8100000000000081ULL,
                           0x800000000000008ULL,
                           0x1000000000000010ULL};
    BB ep = 0x0ULL;
    int flipped = false;
};

struct Move {
    int from = 0;
    int to = 0;
    int promo = 0;
};

const Move no_move{};

struct [[nodiscard]] Stack {
    Move moves[218];
    Move move;
    Move killer;
};

struct [[nodiscard]] TT_Entry {
    BB key;
    Move move;
    int score;
    int depth;
    uint16_t flag;
};

const auto keys = []() {
    mt19937_64 r;

    // pieces from 1-12 multiplied the square + ep squares + castling rights
    // 12 * 64 + 64 + 16 = 848
    array<BB, 848> values;
    for (auto &val : values) {
        val = r();
    }

    return values;
}();

// Engine options
auto num_tt_entries = 64ULL << 15;  // The first value is the size in megabytes
auto thread_count = 1;

vector<TT_Entry> transposition_table;

[[nodiscard]] BB flip(const BB bb) {
    return __builtin_bswap64(bb);
}

[[nodiscard]] auto lsb(const BB bb) {
    return __builtin_ctzll(bb);
}

[[nodiscard]] auto count(const BB bb) {
    return __builtin_popcountll(bb);
}

[[nodiscard]] auto east(const BB bb) {
    return (bb << 1) & ~0x0101010101010101ULL;
}

[[nodiscard]] auto west(const BB bb) {
    return (bb >> 1) & ~0x8080808080808080ULL;
}

[[nodiscard]] BB north(const BB bb) {
    return bb << 8;
}

[[nodiscard]] BB south(const BB bb) {
    return bb >> 8;
}

[[nodiscard]] BB nw(const BB bb) {
    return north(west(bb));
}

[[nodiscard]] BB ne(const BB bb) {
    return north(east(bb));
}

[[nodiscard]] BB sw(const BB bb) {
    return south(west(bb));
}

[[nodiscard]] BB se(const BB bb) {
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
    const BB bb = 1ULL << sq;
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
[[nodiscard]] auto ray(const int sq, const BB blockers, F f) {
    BB mask = f(1ULL << sq);
    mask |= f(mask & ~blockers);
    mask |= f(mask & ~blockers);
    mask |= f(mask & ~blockers);
    mask |= f(mask & ~blockers);
    mask |= f(mask & ~blockers);
    mask |= f(mask & ~blockers);
    mask |= f(mask & ~blockers);
    return mask;
}

[[nodiscard]] BB knight(const int sq, const BB) {
    const BB bb = 1ULL << sq;
    return (((bb << 15) | (bb >> 17)) & 0x7F7F7F7F7F7F7F7FULL) | (((bb << 17) | (bb >> 15)) & 0xFEFEFEFEFEFEFEFEULL) |
           (((bb << 10) | (bb >> 6)) & 0xFCFCFCFCFCFCFCFCULL) | (((bb << 6) | (bb >> 10)) & 0x3F3F3F3F3F3F3F3FULL);
}

[[nodiscard]] auto bishop(const int sq, const BB blockers) {
    return ray(sq, blockers, nw) | ray(sq, blockers, ne) | ray(sq, blockers, sw) | ray(sq, blockers, se);
}

[[nodiscard]] auto rook(const int sq, const BB blockers) {
    return ray(sq, blockers, north) | ray(sq, blockers, east) | ray(sq, blockers, south) | ray(sq, blockers, west);
}

[[nodiscard]] BB king(const int sq, const BB) {
    const BB bb = 1ULL << sq;
    return (bb << 8) | (bb >> 8) | (((bb >> 1) | (bb >> 9) | (bb << 7)) & 0x7F7F7F7F7F7F7F7FULL) |
           (((bb << 1) | (bb << 9) | (bb >> 7)) & 0xFEFEFEFEFEFEFEFEULL);
}

[[nodiscard]] auto attacked(const Position &pos, const int sq, const int them = true) {
    const BB bb = 1ULL << sq;
    const BB kt = pos.colour[them] & pos.pieces[Knight];
    const BB BQ = pos.pieces[Bishop] | pos.pieces[Queen];
    const BB RQ = pos.pieces[Rook] | pos.pieces[Queen];
    const BB pawns = pos.colour[them] & pos.pieces[Pawn];
    const BB pawn_attacks = them ? sw(pawns) | se(pawns) : nw(pawns) | ne(pawns);
    return (pawn_attacks & bb) | (kt & knight(sq, 0)) |
           (bishop(sq, pos.colour[0] | pos.colour[1]) & pos.colour[them] & BQ) |
           (rook(sq, pos.colour[0] | pos.colour[1]) & pos.colour[them] & RQ) |
           (king(sq, 0) & pos.colour[them] & pos.pieces[King]);
}

auto makemove(Position &pos, const Move &move) {
    const int piece = piece_on(pos, move.from);
    const int captured = piece_on(pos, move.to);
    const BB to = 1ULL << move.to;
    const BB from = 1ULL << move.from;

    // Move the piece
    pos.colour[0] ^= from | to;
    pos.pieces[piece] ^= from | to;

    // En passant
    if (piece == Pawn && to == pos.ep) {
        pos.colour[1] ^= to >> 8;
        pos.pieces[Pawn] ^= to >> 8;
    }

    pos.ep = 0x0ULL;

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
        const BB bb = move.to - move.from == 2 ? 0xa0ULL : move.to - move.from == -2 ? 0x9ULL : 0x0ULL;
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

void add_move(Move *const movelist, int &num_moves, const int from, const int to, const int promo = None) {
    movelist[num_moves++] = Move{from, to, promo};
}

void generate_pawn_moves(Move *const movelist, int &num_moves, BB to_mask, const int offset) {
    while (to_mask) {
        const int to = lsb(to_mask);
        to_mask &= to_mask - 1;
        if (to >= 56) {
            add_move(movelist, num_moves, to + offset, to, Queen);
            add_move(movelist, num_moves, to + offset, to, Rook);
            add_move(movelist, num_moves, to + offset, to, Bishop);
            add_move(movelist, num_moves, to + offset, to, Knight);
        } else {
            add_move(movelist, num_moves, to + offset, to);
        }
    }
}

void generate_piece_moves(Move *const movelist,
                          int &num_moves,
                          const Position &pos,
                          const int piece,
                          const BB to_mask,
                          BB (*func)(int, BB)) {
    BB copy = pos.colour[0] & pos.pieces[piece];
    while (copy) {
        const int fr = lsb(copy);
        copy &= copy - 1;
        BB moves = func(fr, pos.colour[0] | pos.colour[1]) & to_mask;
        while (moves) {
            const int to = lsb(moves);
            moves &= moves - 1;
            add_move(movelist, num_moves, fr, to);
        }
    }
}

[[nodiscard]] auto movegen(const Position &pos, Move *const movelist, const bool only_captures) {
    int num_moves = 0;
    const BB all = pos.colour[0] | pos.colour[1];
    const BB to_mask = only_captures ? pos.colour[1] : ~pos.colour[0];
    const BB pawns = pos.colour[0] & pos.pieces[Pawn];
    if (!only_captures) {
        generate_pawn_moves(movelist, num_moves, north(pawns) & ~all, -8);
        generate_pawn_moves(movelist, num_moves, north(north(pawns & 0xFF00ULL) & ~all) & ~all, -16);
    }
    generate_pawn_moves(movelist, num_moves, nw(pawns) & (pos.colour[1] | pos.ep), -7);
    generate_pawn_moves(movelist, num_moves, ne(pawns) & (pos.colour[1] | pos.ep), -9);
    generate_piece_moves(movelist, num_moves, pos, Knight, to_mask, knight);
    generate_piece_moves(movelist, num_moves, pos, Bishop, to_mask, bishop);
    generate_piece_moves(movelist, num_moves, pos, Queen, to_mask, bishop);
    generate_piece_moves(movelist, num_moves, pos, Rook, to_mask, rook);
    generate_piece_moves(movelist, num_moves, pos, Queen, to_mask, rook);
    generate_piece_moves(movelist, num_moves, pos, King, to_mask, king);
    if (!only_captures && pos.castling[0] && !(all & 0x60ULL) && !attacked(pos, 4) && !attacked(pos, 5)) {
        add_move(movelist, num_moves, 4, 6);
    }
    if (!only_captures && pos.castling[1] && !(all & 0xEULL) && !attacked(pos, 4) && !attacked(pos, 3)) {
        add_move(movelist, num_moves, 4, 2);
    }
    return num_moves;
}

[[nodiscard]] int S(const int mg, const int eg) {
    return (eg << 16) + mg;
}

const int phases[] = {0, 1, 1, 2, 4, 0};
const int max_material[] = {133, 418, 401, 603, 1262, 0, 0};
const int material[] = {S(75, 133), S(418, 295), S(401, 330), S(544, 603), S(1262, 1072), 0};
const int psts[][4] = {
    {S(-14, -4), S(-0, -3), S(10, -0), S(4, 7)},
    {S(-23, 1), S(-7, -1), S(10, -0), S(20, -0)},
    {S(-2, -3), S(-4, -0), S(1, -0), S(5, 4)},
    {S(-17, -0), S(2, -10), S(-8, 10), S(23, 1)},
    {S(-4, -31), S(1, -16), S(-28, 19), S(31, 28)},
    {S(-44, 5), S(-2, -6), S(40, -5), S(6, 5)},
};
const int centralities[] = {S(14, -13), S(17, 15), S(24, 7), S(-4, -1), S(-0, 20), S(-23, 11)};
const int outside_files[] = {S(4, -8), S(-2, -4), S(7, -3), S(-3, -3), S(-4, -2), S(-5, 5)};
const int pawn_protection[] = {S(11, 14), S(11, 20), S(-6, 18), S(-3, 13), S(-5, 17), S(-47, 20)};
const int passers[] = {S(17, 8), S(24, -1), S(26, 8), S(28, 30), S(66, 105), S(159, 202)};
const int pawn_doubled = S(-24, -26);
const int pawn_passed_blocked = S(-2, -34);
const int pawn_passed_king_distance[] = {S(1, -5), S(-5, 7)};
const int bishop_pair = S(35, 54);
const int rook_open = S(74, 0);
const int rook_semi_open = S(30, 14);
const int rook_rank78 = S(37, 3);
const int king_shield[] = {S(31, -12), S(17, -17), S(-96, 32)};
const int pawn_attacked[] = {S(-61, -18), S(-53, -42)};

[[nodiscard]] int eval(Position &pos) {
    // Include side to move bonus
    int score = S(10, 10);
    int phase = 0;

    for (int c = 0; c < 2; ++c) {
        // our pawns, their pawns
        const BB pawns[] = {pos.colour[0] & pos.pieces[Pawn], pos.colour[1] & pos.pieces[Pawn]};
        const BB protected_by_pawns = nw(pawns[0]) | ne(pawns[0]);
        const BB attacked_by_pawns = se(pawns[1]) | sw(pawns[1]);
        const int kings[] = {lsb(pos.colour[0] & pos.pieces[King]), lsb(pos.colour[1] & pos.pieces[King])};

        // Bishop pair
        if (count(pos.colour[0] & pos.pieces[Bishop]) == 2) {
            score += bishop_pair;
        }

        // For each piece type
        for (int p = 0; p < 6; ++p) {
            auto copy = pos.colour[0] & pos.pieces[p];
            while (copy) {
                phase += phases[p];

                const int sq = lsb(copy);
                copy &= copy - 1;
                const int rank = sq / 8;
                const int file = sq % 8;
                const int centrality = (7 - abs(7 - rank - file) - abs(rank - file)) / 2;

                // Material
                score += material[p];

                // Centrality
                score += centrality * centralities[p];

                // Closeness to outside files
                score += abs(file - 3) * outside_files[p];

                // Quadrant PSTs
                score += psts[p][(rank / 4) * 2 + file / 4];

                // Pawn protection
                const BB piece_bb = 1ULL << sq;
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
                    BB blockers = 0x101010101010101ULL << sq;
                    blockers = nw(blockers) | ne(blockers);
                    if (!(blockers & pawns[1])) {
                        score += passers[rank - 1];

                        // Blocked passed pawns
                        if (north(piece_bb) & pos.colour[1]) {
                            score += pawn_passed_blocked;
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
                    const BB file_bb = 0x101010101010101ULL << file;
                    if (!(file_bb & pawns[0])) {
                        if (!(file_bb & pawns[1])) {
                            score += rook_open;
                        } else {
                            score += rook_semi_open;
                        }
                    }

                    // Rook on 7th or 8th rank
                    if (rank >= 6) {
                        score += rook_rank78;
                    }
                } else if (p == King && piece_bb & 0xE7) {
                    const BB shield = file < 3 ? 0x700 : 0xE000;
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
    BB hash = pos.flipped;

    // Pieces
    BB copy = pos.colour[0] | pos.colour[1];
    while (copy) {
        const int sq = lsb(copy);
        copy &= copy - 1;
        hash ^= keys[(piece_on(pos, sq) + 6 * ((pos.colour[pos.flipped] >> sq) & 1)) * 64 + sq];
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
              // minify delete on
              int64_t &nodes,
              // minify delete off
              const int64_t stop_time,
              int &stop,
              Stack *const stack,
              int64_t (&hh_table)[2][64][64],
              vector<BB> &hash_history,
              const int do_null = true) {
    const int static_eval = eval(pos);

    // Don't overflow the stack
    if (ply > 127) {
        return static_eval;
    }

    // Check extensions
    const auto in_check = attacked(pos, lsb(pos.colour[0] & pos.pieces[King]));
    depth = in_check ? max(1, depth + 1) : depth;

    const int in_qsearch = depth <= 0;
    if (in_qsearch && static_eval > alpha) {
        if (static_eval >= beta) {
            return beta;
        }
        alpha = static_eval;
    }

    const BB tt_key = get_hash(pos);

    if (ply > 0 && !in_qsearch) {
        // Repetition detection
        for (const auto old_hash : hash_history) {
            if (old_hash == tt_key) {
                return 0;
            }
        }

        if (!in_check && alpha == beta - 1) {
            // Reverse futility pruning
            if (depth < 5) {
                const int margins[] = {0, 50, 100, 200, 300};
                if (static_eval - margins[depth] >= beta) {
                    return beta;
                }
            }

            // Null move pruning
            if (depth > 2 && static_eval >= beta && do_null) {
                auto npos = pos;
                flip(npos);
                npos.ep = 0;
                if (-alphabeta(npos,
                               -beta,
                               -beta + 1,
                               depth - 4 - depth / 6,
                               ply + 1,
                               // minify delete on
                               nodes,
                               // minify delete off
                               stop_time,
                               stop,
                               stack,
                               hh_table,
                               hash_history,
                               false) >= beta) {
                    return beta;
                }
            }

            // Razoring
            if (depth == 1 && static_eval + 200 < alpha) {
                return alphabeta(pos,
                                 alpha,
                                 beta,
                                 0,
                                 ply,
                                 // minify delete on
                                 nodes,
                                 // minify delete off
                                 stop_time,
                                 stop,
                                 stack,
                                 hh_table,
                                 hash_history,
                                 do_null);
            }
        }
    }

    // TT Probing
    TT_Entry &tt_entry = transposition_table[tt_key % num_tt_entries];
    Move tt_move{};
    if (tt_entry.key == tt_key) {
        tt_move = tt_entry.move;
        if (ply > 0 && tt_entry.depth >= depth) {
            if (tt_entry.flag == 0) {
                return tt_entry.score;
            }
            if (tt_entry.flag == 1 && tt_entry.score <= alpha) {
                return tt_entry.score;
            }
            if (tt_entry.flag == 2 && tt_entry.score >= beta) {
                return tt_entry.score;
            }
        }
    }

    // Exit early if out of time
    if (stop || now() >= stop_time) {
        return 0;
    }

    auto &moves = stack[ply].moves;
    const int num_moves = movegen(pos, moves, in_qsearch);

    // Score moves
    int64_t move_scores[256];
    for (int j = 0; j < num_moves; ++j) {
        const int capture = piece_on(pos, moves[j].to);
        if (moves[j] == tt_move) {
            move_scores[j] = 1LL << 62;
        } else if (capture != None) {
            move_scores[j] = ((capture + 1) * (1LL << 54)) - piece_on(pos, moves[j].from);
        } else if (moves[j] == stack[ply].killer) {
            move_scores[j] = 1LL << 50;
        } else {
            move_scores[j] = hh_table[pos.flipped][moves[j].from][moves[j].to];
        }
    }

    int moves_evaluated = 0;
    int best_score = -INF;
    Move best_move{};
    uint16_t tt_flag = 1;  // Alpha flag
    hash_history.emplace_back(tt_key);
    for (int i = 0; i < num_moves; ++i) {
        // Find best move remaining
        int best_move_index = i;
        for (int j = i; j < num_moves; ++j) {
            if (move_scores[j] > move_scores[best_move_index]) {
                best_move_index = j;
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

        // minify delete on
        nodes++;
        // minify delete off

        int score;
        if (in_qsearch || !moves_evaluated) {
        full_window:
            score = -alphabeta(npos,
                               -beta,
                               -alpha,
                               depth - 1,
                               ply + 1,
                               // minify delete on
                               nodes,
                               // minify delete off
                               stop_time,
                               stop,
                               stack,
                               hh_table,
                               hash_history);
        } else {
            // Zero window search with late move reduction
            score = -alphabeta(npos,
                               -alpha - 1,
                               -alpha,
                               depth - (depth > 3 && moves_evaluated > 3 ? 2 + moves_evaluated / 16 : 1),
                               ply + 1,
                               // minify delete on
                               nodes,
                               // minify delete off
                               stop_time,
                               stop,
                               stack,
                               hh_table,
                               hash_history);
            if (score > alpha && score < beta) {
                goto full_window;
            }
        }

        // Exit early if out of time
        if (stop || now() >= stop_time) {
            hash_history.pop_back();
            return 0;
        }

        moves_evaluated++;

        if (score > best_score) {
            best_score = score;
            best_move = move;
            if (score > alpha) {
                tt_flag = 0;  // Exact flag
                alpha = score;
                stack[ply].move = move;
            }
        } else if (!in_qsearch && !in_check && alpha == beta - 1 && depth <= 3 && moves_evaluated >= (depth * 3) + 2 &&
                   static_eval < alpha - (50 * depth) && best_move_score < (1LL << 50)) {
            best_score = alpha;
            break;
        }

        if (alpha >= beta) {
            tt_flag = 2;  // Beta flag
            const int capture = piece_on(pos, move.to);
            if (capture == None) {
                hh_table[pos.flipped][move.from][move.to] += depth * depth;
                stack[ply].killer = move;
            }
            break;
        }
    }
    hash_history.pop_back();

    // Return mate or draw scores if no moves found
    if (best_score == -INF) {
        return in_qsearch ? alpha : in_check ? ply - MATE_SCORE : 0;
    }

    // Save to TT
    if (tt_entry.key != tt_key || depth >= tt_entry.depth || tt_flag == 0) {
        tt_entry =
            TT_Entry{tt_key, best_move == no_move ? tt_move : best_move, best_score, in_qsearch ? 0 : depth, tt_flag};
    }

    return alpha;
}

// minify delete on
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
// minify delete off

// minify delete on
void print_pv(const Position &pos, const Move move, vector<BB> &hash_history) {
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
    const BB tt_key = get_hash(npos);
    const TT_Entry &tt_entry = transposition_table[tt_key % num_tt_entries];

    // Only continue if the move was valid and comes from a PV search
    if (tt_entry.key != tt_key || tt_entry.move == Move{} || tt_entry.flag != 0) {
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
// minify delete off

auto iteratively_deepen(Position &pos,
                        vector<BB> &hash_history,
                        // minify delete on
                        int thread_id,
                        const bool is_bench,
                        // minify delete off
                        const int64_t start_time,
                        const int allocated_time,
                        int &stop) {
    Stack stack[128] = {};
    int64_t hh_table[2][64][64] = {};
    // minify delete on
    int64_t nodes = 0;
    // minify delete off

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
                                        // minify delete on
                                        nodes,
                                        // minify delete off
                                        start_time + allocated_time,
                                        stop,
                                        stack,
                                        hh_table,
                                        hash_history);

        // Hard time limit exceeded
        if (now() >= start_time + allocated_time || stop) {
            break;
        }

        // minify delete on
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
            cout << endl;

            // OpenBench compliance
            if (is_bench && i >= 10) {
                cout << "Bench: ";
                cout << elapsed << " ms ";
                cout << nodes << " nodes ";
                cout << nodes * 1000 / max(elapsed, static_cast<int64_t>(1)) << " nps";
                cout << endl;
                break;
            }
        }
        // minify delete off

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

// minify delete on
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
// minify delete off

int main(
    // minify delete on
    const int argc,
    const char **argv
    // minify delete off
) {
    setbuf(stdout, 0);
    Position pos;
    vector<BB> hash_history;
    Move moves[256];

    // minify delete on
    // OpenBench compliance
    if (argc > 1 && argv[1] == string("bench")) {
        // Initialise the TT
        transposition_table.resize(num_tt_entries);

        int stop = false;
        iteratively_deepen(pos, hash_history, 0, true, now(), 1 << 30, stop);

        return 0;
    }
    // minify delete off

    // Wait for "uci"
    getchar();

    // Send UCI info
    puts("id name 4ku");
    puts("id author kz04px");
    // minify delete on
    cout << "option name Threads type spin default " << thread_count << " min 1 max 256\n";
    cout << "option name Hash type spin default " << (num_tt_entries >> 15) << " min 1 max 65536\n";
    // minify delete off
    puts("uciok");

    // Initialise the TT
    transposition_table.resize(num_tt_entries);

    while (true) {
        string word;
        cin >> word;
        if (word == "quit"
            // minify delete on
            || !cin.good()
            // minify delete off
        ) {
            break;
        } else if (word == "ucinewgame") {
            memset(transposition_table.data(), 0, sizeof(TT_Entry) * transposition_table.size());
        } else if (word == "isready") {
            puts("readyok");
        }
        // minify delete on
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
        // minify delete off
        else if (word == "go") {
            int wtime;
            int btime;
            cin >> word >> wtime >> word >> btime;
            const auto start = now();
            const auto allocated_time = (pos.flipped ? btime : wtime) / 3;

            // Lazy SMP
            vector<thread> threads;
            vector<int> stops(thread_count, false);
            for (int i = 1; i < thread_count; ++i) {
                threads.emplace_back([=, &stops]() mutable {
                    iteratively_deepen(pos,
                                       hash_history,
                                       // minify delete on
                                       i,
                                       false,
                                       // minify delete off
                                       start,
                                       1 << 30,
                                       stops[i]);
                });
            }
            const auto best_move = iteratively_deepen(pos,
                                                      hash_history,
                                                      // minify delete on
                                                      0,
                                                      false,
                                                      // minify delete off
                                                      start,
                                                      allocated_time,
                                                      stops[0]);
            for (int i = 1; i < thread_count; ++i) {
                stops[i] = true;
            }
            for (int i = 1; i < thread_count; ++i) {
                threads[i - 1].join();
            }

            cout << "bestmove " << move_str(best_move, pos.flipped) << endl;
        } else if (word == "position") {
            // Set to startpos
            pos = Position();
            hash_history.clear();

            // minify delete on
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
            // minify delete off
        } else {
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
