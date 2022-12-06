#include <array>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <ctime>
#include <iostream>
#include <string>
#include <thread>
#include <vector>

#define MATE_SCORE (1 << 15)
#define INF (1 << 16)

using namespace std;

[[nodiscard]] long long int now() {
    timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec * 1000 + t.tv_nsec / 1000000;
}

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

struct Move {
    int from = 0;
    int to = 0;
    int promo = 0;
};

using BB = uint64_t;

struct [[nodiscard]] Position {
    array<BB, 2> colour = {0xFFFFULL, 0xFFFF000000000000ULL};
    array<BB, 6> pieces = {0xFF00000000FF00ULL,
                           0x4200000000000042ULL,
                           0x2400000000000024ULL,
                           0x8100000000000081ULL,
                           0x800000000000008ULL,
                           0x1000000000000010ULL};
    BB ep = 0x0ULL;
    array<int, 4> castling = {true, true, true, true};
    int flipped = false;
};

struct [[nodiscard]] Stack {
    Move move;
    Move killer;
};

struct [[nodiscard]] TT_Entry {
    uint64_t key;
    Move move;
    int score;
    int depth;
    uint16_t flag;
};

const auto keys = []() {
    // pieces from 1-12 multiplied the square + ep squares + castling rights
    array<uint64_t, 12 * 64 + 64 + 16> values{};
    for (auto &val : values) {
        for (int i = 0; i < 64; ++i) {
            val = val * 2 + rand() % 2;
        }
    }

    return values;
}();

// Engine options
int num_tt_entries = 64 << 15;  // The first value is the size in megabytes
int thread_count = 1;

vector<TT_Entry> transposition_table;

[[nodiscard]] BB flip(const BB bb) {
    return __builtin_bswap64(bb);
}

[[nodiscard]] int lsb(const BB bb) {
    return __builtin_ctzll(bb);
}

[[nodiscard]] int count(const BB bb) {
    return __builtin_popcountll(bb);
}

[[nodiscard]] BB north(const BB bb) {
    return bb << 8;
}

[[nodiscard]] BB south(const BB bb) {
    return bb >> 8;
}

[[nodiscard]] BB east(const BB bb) {
    return (bb << 1) & ~0x0101010101010101ULL;
}

[[nodiscard]] BB west(const BB bb) {
    return (bb >> 1) & ~0x8080808080808080ULL;
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

[[nodiscard]] bool operator==(const Move &lhs, const Move &rhs) {
    return lhs.from == rhs.from && lhs.to == rhs.to && lhs.promo == rhs.promo;
}

[[nodiscard]] string move_str(const Move &move, const int flip) {
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
[[nodiscard]] BB ray(const int sq, const BB blockers, F f) {
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

[[nodiscard]] BB bishop(const int sq, const BB blockers) {
    return ray(sq, blockers, nw) | ray(sq, blockers, ne) | ray(sq, blockers, sw) | ray(sq, blockers, se);
}

[[nodiscard]] BB rook(const int sq, const BB blockers) {
    return ray(sq, blockers, north) | ray(sq, blockers, east) | ray(sq, blockers, south) | ray(sq, blockers, west);
}

[[nodiscard]] BB king(const int sq, const BB) {
    const BB bb = 1ULL << sq;
    return (bb << 8) | (bb >> 8) | (((bb >> 1) | (bb >> 9) | (bb << 7)) & 0x7F7F7F7F7F7F7F7FULL) |
           (((bb << 1) | (bb << 9) | (bb >> 7)) & 0xFEFEFEFEFEFEFEFEULL);
}

[[nodiscard]] bool attacked(const Position &pos, const int sq, const int them = true) {
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

int makemove(Position &pos, const Move &move) {
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
    movelist[num_moves] = Move{from, to, promo};
    num_moves++;
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

[[nodiscard]] int movegen(const Position &pos, Move *const movelist, const bool only_captures) {
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
const int material[] = {S(35, 133), S(422, 292), S(410, 330), S(552, 601), S(1274, 1075)};
const int psts[][16] = {
    {S(-15, 8),
     S(0, -3),
     S(21, -5),
     S(7, -5),
     S(-13, 1),
     S(-24, -14),
     S(-25, -7),
     S(2, -10),
     S(14, 16),
     S(-0, -5),
     S(-8, -7),
     S(6, 9),
     S(49, 27),
     S(30, -24),
     S(51, -30),
     S(-95, 48)},
    {S(-19, -20),
     S(-13, 1),
     S(-0, 5),
     S(-2, -18),
     S(-16, 12),
     S(-14, 1),
     S(-0, 2),
     S(3, 13),
     S(17, 9),
     S(20, 6),
     S(19, 9),
     S(35, 12),
     S(-100, 10),
     S(50, -12),
     S(38, -11),
     S(-17, -20)},
    {S(12, -14),
     S(6, -12),
     S(6, -1),
     S(16, -7),
     S(12, -0),
     S(-12, 2),
     S(-19, 5),
     S(9, 2),
     S(4, 8),
     S(-4, -2),
     S(-1, 2),
     S(3, 14),
     S(-2, -1),
     S(-43, 0),
     S(29, -5),
     S(-17, 8)},
    {S(-21, -1),
     S(-10, -6),
     S(1, -10),
     S(-10, -17),
     S(-48, 5),
     S(-26, -2),
     S(-5, -10),
     S(-11, -11),
     S(-33, 15),
     S(-2, 7),
     S(14, 1),
     S(6, 2),
     S(16, 14),
     S(42, 4),
     S(70, -6),
     S(17, 15)},
    {S(-17, -27),
     S(12, -59),
     S(6, -41),
     S(-6, -47),
     S(-0, -28),
     S(-13, -13),
     S(-3, -7),
     S(10, 21),
     S(-29, 8),
     S(-24, 6),
     S(9, 31),
     S(19, 52),
     S(-44, 22),
     S(-16, 39),
     S(39, 25),
     S(58, 17)},
    {S(2, -21),
     S(-93, 22),
     S(-48, 12),
     S(23, -14),
     S(2, -11),
     S(-60, -2),
     S(-42, -3),
     S(-12, 1),
     S(30, 8),
     S(16, -13),
     S(31, -11),
     S(15, 19),
     S(121, -27),
     S(49, -4),
     S(-16, 25),
     S(-17, 18)},
};

const int centralities[] = {S(35, -12), S(14, 14), S(27, 4), S(-3, -2), S(5, 13), S(-30, 28)};
const int outside_files[] = {S(13, -8), S(-3, -5), S(3, -5), S(1, -4), S(-0, -6), S(-21, 6)};
const int pawn_protection[] = {S(16, 13), S(13, 18), S(1, 14), S(3, 10), S(-4, 4), S(-49, 20)};
const int passers[] = {S(17, 1), S(8, 12), S(-3, 26), S(-10, 38), S(25, 107), S(122, 195)};
const int pawn_doubled = S(-16, -29);
const int pawn_passed_blocked = S(3, -47);
const int bishop_pair = S(33, 56);
const int rook_open = S(74, 0);
const int rook_semi_open = S(30, 12);
const int king_shield[] = {S(15, -9), S(5, -8)};

[[nodiscard]] int eval(Position &pos) {
    // Include side to move bonus
    int score = S(10, 10);
    int phase = 0;

    for (int c = 0; c < 2; ++c) {
        // our pawns, their pawns
        const BB pawns[] = {pos.colour[0] & pos.pieces[Pawn], pos.colour[1] & pos.pieces[Pawn]};
        const BB protected_by_pawns = nw(pawns[0]) | ne(pawns[0]);

        // Bishop pair
        if (count(pos.colour[0] & pos.pieces[Bishop]) == 2) {
            score += bishop_pair;
        }

        // For each piece type
        for (int p = 0; p < 6; ++p) {
            BB copy = pos.colour[0] & pos.pieces[p];
            while (copy) {
                phase += phases[p];

                const int sq = lsb(copy);
                copy &= copy - 1;
                const int rank = sq / 8;
                const int file = sq % 8;
                const int centrality = (7 - abs(7 - rank - file) - abs(rank - file)) / 2;

                // Material
                score += material[p];

                // PSTs compressed into 2x2 blocks
                score += psts[p][(rank / 2) * 4 + file / 2];

                // Centrality
                score += centrality * centralities[p];

                // Closeness to outside files
                score += abs(file - 3) * outside_files[p];

                // Pawn protection
                const BB piece_bb = 1ULL << sq;
                if (piece_bb & protected_by_pawns) {
                    score += pawn_protection[p];
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
                } else if (p == King && piece_bb & 0xE7) {
                    const BB shield = file < 3 ? 0x700 : 0xE000;
                    score += count(shield & pawns[0]) * king_shield[0];
                    score += count(north(shield) & pawns[0]) * king_shield[1];
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
    uint64_t hash = 0;

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
              int64_t (&hh_table)[64][64],
              vector<uint64_t> &hash_history,
              const int do_null = true) {
    const auto in_check = attacked(pos, lsb(pos.colour[0] & pos.pieces[King]));
    const int static_eval = eval(pos);

    // Check extensions
    depth += in_check;

    const int in_qsearch = depth <= 0;

    // TT probing
    const uint64_t tt_key = in_qsearch ? 0 : get_hash(pos);
    TT_Entry &tt_entry = transposition_table[tt_key % num_tt_entries];
    Move tt_move{};

    if (in_qsearch) {
        if (static_eval >= beta) {
            return beta;
        }
        if (alpha < static_eval) {
            alpha = static_eval;
        }
    } else if (ply > 0) {
        // Repetition detection
        for (const auto old_hash : hash_history) {
            if (old_hash == tt_key) {
                return 0;
            }
        }

        // Reverse futility pruning
        if (depth < 3) {
            const int margin = 120;
            if (static_eval - margin * depth >= beta) {
                return beta;
            }
        }
        // Null move pruning
        else if (!in_check && static_eval >= beta && do_null) {
            auto npos = pos;
            flip(npos);
            npos.ep = 0;
            if (-alphabeta(npos,
                           -beta,
                           -beta + 1,
                           depth - 3,
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
        if (depth == 1 && !in_check && static_eval + 300 < alpha) {
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

    // TT Probing
    if (tt_entry.key == tt_key) {
        tt_move = tt_entry.move;
        if (!in_qsearch && ply > 0 && tt_entry.depth >= depth) {
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

    Move moves[256];
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
            move_scores[j] = hh_table[moves[j].from][moves[j].to];
        }
    }

    int moves_evaluated = 0;
    int best_score = -INF;
    Move best_move{};
    uint16_t tt_flag = 1;  // Alpha flag
    hash_history.push_back(tt_key);
    for (int i = 0; i < num_moves; ++i) {
        // Find best move remaining
        int best_move_index = i;
        for (int j = i; j < num_moves; ++j) {
            if (move_scores[j] > move_scores[best_move_index]) {
                best_move_index = j;
            }
        }

        const auto move = moves[best_move_index];
        moves[best_move_index] = moves[i];
        move_scores[best_move_index] = move_scores[i];

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
                               depth - (depth > 3 && moves_evaluated > 3) - 1,
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
        moves_evaluated++;

        // Exit early if out of time
        if (stop || now() >= stop_time) {
            hash_history.pop_back();
            return 0;
        }

        if (score > best_score) {
            best_score = score;
            best_move = move;
            if (score > alpha) {
                tt_flag = 0;  // Exact flag
                alpha = score;
                stack[ply].move = move;
            }
        }

        if (alpha >= beta) {
            tt_flag = 2;  // Beta flag
            const int capture = piece_on(pos, move.to);
            if (capture == None) {
                hh_table[move.from][move.to] += depth * depth;
                stack[ply].killer = move;
            }
            break;
        }
    }
    hash_history.pop_back();

    // Return mate or draw scores if no moves found and not in qsearch
    if (!in_qsearch && best_score == -INF) {
        return in_check ? ply - MATE_SCORE : 0;
    }

    // Save to TT
    if (!in_qsearch && (tt_entry.key != tt_key || depth >= tt_entry.depth || tt_flag == 0)) {
        tt_entry = TT_Entry{tt_key, best_move, best_score, depth, tt_flag};
    }

    return alpha;
}

Move iteratively_deepen(Position &pos,
                        vector<uint64_t> &hash_history,
                        // minify delete on
                        int thread_id,
                        // minify delete off
                        const int64_t start_time,
                        const int allocated_time,
                        int &stop) {
    Stack stack[128] = {};
    int64_t hh_table[64][64] = {};
    // minify delete on
    int64_t nodes = 0;
    // minify delete off

    for (int i = 1; i < 128; ++i) {
        // minify delete on
        const int score =
            // minify delete off
            alphabeta(pos,
                      -INF,
                      INF,
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

        if (stop || now() >= start_time + allocated_time / 10) {
            break;
        }

        // minify delete on
        if (thread_id == 0) {
            const auto elapsed = now() - start_time;

            cout << "info";
            cout << " depth " << i;
            cout << " score cp " << score;
            cout << " time " << elapsed;
            cout << " nodes " << nodes;
            if (elapsed > 0) {
                cout << " nps " << nodes * 1000 / elapsed;
            }
            cout << " pv " << move_str(stack[0].move, pos.flipped);
            cout << endl;
        }
        // minify delete off
    }
    return stack[0].move;
}

int main() {
    setbuf(stdout, NULL);
    Position pos;
    vector<uint64_t> hash_history;
    Move moves[256];

    // Wait for "uci"
    getchar();

    // Send UCI info
    puts("id name 4ku");
    puts("id author kz04px");
    // minify delete on
    cout << "option name Threads type spin default " << thread_count << " min 1 max 256\n";
    cout << "option name Hash type spin default " << (num_tt_entries >> 15) << " min 1 max 1024\n";
    // minify delete off
    puts("uciok");

    // Initialise the TT
    transposition_table.resize(num_tt_entries);
    memset(transposition_table.data(), 0, sizeof(TT_Entry) * transposition_table.size());

    while (true) {
        string word;
        cin >> word;
        if (word == "quit") {
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
                num_tt_entries = min(max(num_tt_entries, 1), 1024) * 1024 * 1024 / sizeof(TT_Entry);
                transposition_table.resize(num_tt_entries);
                memset(transposition_table.data(), 0, sizeof(TT_Entry) * transposition_table.size());
            }
        }
        // minify delete off
        else if (word == "go") {
            int wtime;
            int btime;
            cin >> word;
            cin >> wtime;
            cin >> word;
            cin >> btime;
            const auto start = now();
            const auto allocated_time = (pos.flipped ? btime : wtime) / 4;

            // Lazy SMP
            vector<thread> threads;
            vector<int> stops(thread_count, false);
            for (int i = 1; i < thread_count; ++i) {
                threads.emplace_back([=, &stops]() mutable {
                    iteratively_deepen(pos,
                                       hash_history,
                                       // minify delete on
                                       i,
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

            cout << "bestmove " << move_str(best_move, pos.flipped) << "\n";
        } else if (word == "position") {
            pos = Position();
            hash_history.clear();
        } else {
            const int num_moves = movegen(pos, moves, false);
            for (int i = 0; i < num_moves; ++i) {
                if (word == move_str(moves[i], pos.flipped)) {
                    if (piece_on(pos, moves[i].to) != None || piece_on(pos, moves[i].from) == Pawn) {
                        hash_history.clear();
                    } else {
                        hash_history.push_back(get_hash(pos));
                    }

                    makemove(pos, moves[i]);
                    break;
                }
            }
        }
    }
}
