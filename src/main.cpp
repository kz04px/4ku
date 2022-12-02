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
const int MAX_TT_SIZE = 2000000;
const int thread_count = 1;

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
                          BB (*func)(int, BB)) {
    BB copy = pos.colour[0] & pos.pieces[piece];
    while (copy) {
        const int fr = lsb(copy);
        copy &= copy - 1;
        BB moves = func(fr, pos.colour[0] | pos.colour[1]) & ~pos.colour[0];
        while (moves) {
            const int to = lsb(moves);
            moves &= moves - 1;
            add_move(movelist, num_moves, fr, to);
        }
    }
}

[[nodiscard]] int movegen(const Position &pos, Move *const movelist) {
    int num_moves = 0;
    const BB all = pos.colour[0] | pos.colour[1];
    const BB pawns = pos.colour[0] & pos.pieces[Pawn];
    generate_pawn_moves(movelist, num_moves, north(pawns) & ~all, -8);
    generate_pawn_moves(movelist, num_moves, north(north(pawns & 0xFF00ULL) & ~all) & ~all, -16);
    generate_pawn_moves(movelist, num_moves, nw(pawns) & (pos.colour[1] | pos.ep), -7);
    generate_pawn_moves(movelist, num_moves, ne(pawns) & (pos.colour[1] | pos.ep), -9);
    generate_piece_moves(movelist, num_moves, pos, Knight, knight);
    generate_piece_moves(movelist, num_moves, pos, Bishop, bishop);
    generate_piece_moves(movelist, num_moves, pos, Queen, bishop);
    generate_piece_moves(movelist, num_moves, pos, Rook, rook);
    generate_piece_moves(movelist, num_moves, pos, Queen, rook);
    generate_piece_moves(movelist, num_moves, pos, King, king);
    if (pos.castling[0] && !(all & 0x60ULL) && !attacked(pos, 4) && !attacked(pos, 5)) {
        add_move(movelist, num_moves, 4, 6);
    }
    if (pos.castling[1] && !(all & 0xEULL) && !attacked(pos, 4) && !attacked(pos, 3)) {
        add_move(movelist, num_moves, 4, 2);
    }
    return num_moves;
}

[[nodiscard]] int S(const int mg, const int eg) {
    return (eg << 16) + mg;
}

const int phases[] = {0, 1, 1, 2, 4, 0};
const int material[] = {S(65, 127), S(392, 297), S(400, 328), S(534, 602), S(1246, 1061)};
const int centralities[] = {S(18, -13), S(23, 15), S(22, 7), S(-4, -0), S(-2, 27), S(-32, 24)};
const int outside_files[] = {S(7, -6), S(4, -5), S(7, -3), S(-2, -3), S(-3, 6), S(5, -1)};
const int pawn_protection[] = {S(9, 14), S(5, 22), S(-7, 17), S(-4, 14), S(-7, 14), S(0, 0)};
const int passers[] = {S(13, 7), S(3, 11), S(-12, 27), S(3, 49), S(30, 120), S(115, 212)};
const int pawn_doubled = S(-23, -28);
const int pawn_passed_blocked = S(6, -48);
const int bishop_pair = S(34, 57);
const int rook_open = S(73, 2);
const int rook_semi_open = S(33, 10);
const int rook_rank78 = S(47, 10);
const int king_shield[] = {S(23, -10), S(10, -15)};

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

                    // Rook on 7th or 8th rank
                    if (rank >= 6) {
                        score += rook_rank78;
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
    BB copy = pos.colour[0] | pos.colour[1];
    while (copy) {
        const int sq = lsb(copy);
        copy &= copy - 1;
        hash ^= keys[(piece_on(pos, sq) + 6 * ((pos.colour[pos.flipped] >> sq) & 1)) * 64 + sq];
    }
    if (pos.ep) {
        hash ^= keys[768 + lsb(pos.ep)];
    }
    hash ^= keys[832 + (pos.castling[0] | pos.castling[1] << 1 | pos.castling[2] << 2 | pos.castling[3] << 3)];

    return hash;
}

int alphabeta(Position &pos,
              int alpha,
              const int beta,
              int depth,
              const int ply,
              const int64_t stop_time,
              int &stop,
              Stack *const stack,
              uint64_t (&hh_table)[64][64],
              vector<uint64_t> &hash_history,
              const int do_null = true) {
    const auto in_check = attacked(pos, lsb(pos.colour[0] & pos.pieces[King]));
    const int static_eval = eval(pos);

    // Check extensions
    depth += in_check;

    const int in_qsearch = depth <= 0;

    // TT probing
    const uint64_t tt_key = in_qsearch ? 0 : get_hash(pos);
    TT_Entry &tt_entry = transposition_table[tt_key % MAX_TT_SIZE];
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
            return alphabeta(pos, alpha, beta, 0, ply, stop_time, stop, stack, hh_table, hash_history, do_null);
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
    const int num_moves = movegen(pos, moves);

    // Score moves
    int move_scores[256];
    for (int j = 0; j < num_moves; ++j) {
        int move_score = 0;
        const int capture = piece_on(pos, moves[j].to);
        if (!in_qsearch && moves[j] == tt_move) {
            move_score = 1 << 16;
        } else {
            if (capture != None) {
                move_score = ((capture + 1) * (1 << 10)) - piece_on(pos, moves[j].from);
            } else if (moves[j] == stack[ply].killer) {
                move_score = 1 << 8;
            }
        }
        move_scores[j] = move_score;
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
            } else if (move_scores[j] == move_scores[best_move_index]) {
                if (hh_table[moves[j].from][moves[j].to] >
                    hh_table[moves[best_move_index].from][moves[best_move_index].to]) {
                    best_move_index = j;
                }
            }
        }

        const auto move = moves[best_move_index];
        moves[best_move_index] = moves[i];
        move_scores[best_move_index] = move_scores[i];

        // qsearch needs captures only
        if (in_qsearch && piece_on(pos, move.to) == None) {
            break;
        }

        auto npos = pos;
        if (!makemove(npos, move)) {
            continue;
        }

        int score;
        if (in_qsearch || !moves_evaluated) {
        full_window:
            score = -alphabeta(npos, -beta, -alpha, depth - 1, ply + 1, stop_time, stop, stack, hh_table, hash_history);
        } else {
            // Zero window search with late move reduction
            score = -alphabeta(npos,
                               -alpha - 1,
                               -alpha,
                               depth - (depth > 3 && moves_evaluated > 3) - 1,
                               ply + 1,
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
    uint64_t hh_table[64][64] = {};

    for (int i = 1; i < 128; ++i) {
        // minify delete on
        const int score =
            // minify delete off
            alphabeta(pos, -INF, INF, i, 0, start_time + allocated_time, stop, stack, hh_table, hash_history);

        if (stop || now() >= start_time + allocated_time / 10) {
            break;
        }

        // minify delete on
        if (thread_id == 0) {
            cout << "info";
            cout << " depth " << i;
            cout << " score cp " << score;
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
    getchar();
    puts("id name 4ku\nid author kz04px\nuciok");
    transposition_table.resize(MAX_TT_SIZE);
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
        } else if (word == "go") {
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
            vector<int> stops = {false};
            for (int i = 1; i < thread_count; ++i) {
                stops.emplace_back(false);
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
            const int num_moves = movegen(pos, moves);
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
