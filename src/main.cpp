#include <cstdint>
#include <cstdio>
#include <ctime>
#include <iostream>
#include <string>

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

struct [[nodiscard]] Move {
    int from;
    int to;
    int promo;
};

using BB = uint64_t;

struct [[nodiscard]] Position {
    BB colour[2] = {0xFFFFULL, 0xFFFF000000000000ULL};
    BB pieces[6] = {0xFF00000000FF00ULL,
                    0x4200000000000042ULL,
                    0x2400000000000024ULL,
                    0x8100000000000081ULL,
                    0x800000000000008ULL,
                    0x1000000000000010ULL};
    BB ep = 0x0ULL;
    bool castling[4] = {true, true, true, true};
    bool flipped = false;
};

struct [[nodiscard]] Stack {
    Move move;
    Move killer;
};

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

[[nodiscard]] string move_str(const Move &move, const bool flip) {
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

[[nodiscard]] bool attacked(const Position &pos, const int sq, const bool them = true) {
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

bool makemove(Position &pos, const Move &move) {
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
    const int ksq = lsb(pos.colour[1] & pos.pieces[King]);
    return !attacked(pos, ksq, false);
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
    if (pos.castling[0] && !(all & (0x60ULL)) && !attacked(pos, 4) && !attacked(pos, 5)) {
        add_move(movelist, num_moves, 4, 6);
    }
    if (pos.castling[1] && !(all & (0xEULL)) && !attacked(pos, 4) && !attacked(pos, 3)) {
        add_move(movelist, num_moves, 4, 2);
    }
    return num_moves;
}

const int material[] = {100, 339, 372, 582, 1180};
const int centralities[] = {2, 20, 16, 1, 3, 11};
const int passers[] = {17, 11, 13, 31, 93, 192};
const int rook_semi_open = 25;
const int rook_open = 35;
const int rook_rank78 = 24;

[[nodiscard]] int eval(Position &pos) {
    // Include side to move bonus
    int score = 10;

    for (int c = 0; c < 2; ++c) {
        // our pawns, their pawns
        const BB pawns[] = {pos.colour[0] & pos.pieces[Pawn], pos.colour[1] & pos.pieces[Pawn]};

        // For each piece type
        for (int p = 0; p < 6; ++p) {
            BB copy = pos.colour[0] & pos.pieces[p];
            while (copy) {
                const int sq = lsb(copy);
                copy &= copy - 1;
                const int rank = sq / 8;
                const int file = sq % 8;
                const int centrality = (7 - abs(7 - rank - file) - abs(rank - file)) / 2;

                // Material
                score += material[p];

                // Centrality
                score += centrality * centralities[p];

                if (p == Pawn) {
                    // Passed pawns
                    BB blockers = 0x101010101010101ULL << sq;
                    blockers = nw(blockers) | ne(blockers);
                    if ((blockers & pawns[1]) == 0) {
                        score += passers[rank - 1];
                    }
                } else if (p == Rook) {
                    // Rook on open or semi-open files
                    const BB file_bb = 0x101010101010101ULL << file;
                    if ((file_bb & pawns[0]) == 0) {
                        if ((file_bb & pawns[1]) == 0) {
                            score += rook_open;
                        } else {
                            score += rook_semi_open;
                        }
                    }

                    // Rook on 7th or 8th rank
                    if (rank >= 6) {
                        score += rook_rank78;
                    }
                }
            }
        }

        flip(pos);

        score = -score;
    }
    return score;
}

int alphabeta(Position &pos,
              int alpha,
              const int beta,
              int depth,
              const int ply,
              const long long int stop_time,
              Stack *const stack) {
    const int ksq = lsb(pos.colour[0] & pos.pieces[King]);
    const auto in_check = attacked(pos, ksq);
    const int static_eval = eval(pos);

    // Check extensions
    depth += in_check;

    const bool in_qsearch = depth <= 0;
    if (in_qsearch) {
        if (static_eval >= beta) {
            return beta;
        }
        if (alpha < static_eval) {
            alpha = static_eval;
        }
    }
    // Reverse futility pruning
    else if (depth < 3) {
        const int margin = 120;
        if (static_eval - margin * depth >= beta) {
            return beta;
        }
    }
    // Null move pruning
    else if (!in_check && static_eval >= beta && beta - alpha > 1) {
    	auto npos = pos;
    	flip(npos);
    	npos.ep = 0;
    	if (-alphabeta(npos, -beta, -beta + 1, depth - 3, ply + 1, stop_time, stack) >= beta) {
    		return beta;
    	}
    }

    // Exit early if out of time
    if (now() >= stop_time) {
        return 0;
    }

    Move moves[256];
    const int num_moves = movegen(pos, moves);

    // Score moves
    int move_scores[256];
    for (int j = 0; j < num_moves; ++j) {
        int move_score = 0;
        if (!in_qsearch && moves[j] == stack[ply].move) {
            move_score = 1 << 16;
        } else {
            const int capture = piece_on(pos, moves[j].to);
            if (capture != None) {
                move_score = ((capture + 1) * (1 << 10)) - piece_on(pos, moves[j].from);
            } else if (moves[j] == stack[ply].killer) {
                move_score = 1 << 8;
            }
        }
        move_scores[j] = move_score;
    }

    int best_score = -INF;
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

        // qsearch needs captures only
        if (in_qsearch && piece_on(pos, move.to) == None) {
            break;
        }

        auto npos = pos;
        if (!makemove(npos, move)) {
            continue;
        }

        const int score = -alphabeta(npos, -beta, -alpha, depth - 1, ply + 1, stop_time, stack);

        if (score > best_score) {
            best_score = score;
            if (score > alpha) {
                alpha = score;
                stack[ply].move = move;
            }
        }

        if (alpha >= beta) {
            const int capture = piece_on(pos, move.to);
            if (capture == None) {
                stack[ply].killer = move;
            }
            break;
        }
    }

    // Return mate or draw scores if no moves found and not in qsearch
    if (!in_qsearch && best_score == -INF) {
        return in_check ? -MATE_SCORE : 0;
    }

    return alpha;
}

int main() {
    setbuf(stdout, NULL);
    Position pos;
    Move moves[256];
    getchar();
    puts("id name 4ku2\nid author kz04px\nuciok");
    while (true) {
        string word;
        cin >> word;
        if (word == "quit") {
            break;
        } else if (word == "isready") {
            puts("readyok");
        } else if (word == "go") {
            int wtime;
            int btime;
            cin >> word;
            cin >> wtime;
            cin >> word;
            cin >> btime;
            const auto stop_time = now() + (pos.flipped ? btime : wtime) / 30;
            string bestmove_str;
            Stack stack[128];
            for (int i = 1; i < 128; ++i) {
                alphabeta(pos, -INF, INF, i, 0, stop_time, stack);
                if (now() >= stop_time) {
                    break;
                }
                bestmove_str = move_str(stack[0].move, pos.flipped);
            }
            cout << "bestmove " << bestmove_str << "\n";
        } else if (word == "position") {
            pos = Position();
        } else {
            const int num_moves = movegen(pos, moves);
            for (int i = 0; i < num_moves; ++i) {
                if (word == move_str(moves[i], pos.flipped)) {
                    makemove(pos, moves[i]);
                    break;
                }
            }
        }
    }
}
