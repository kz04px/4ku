#include <cstdint>
#include <cstdio>
#include <ctime>
#include <iostream>
#include <string>

#define MATE_SCORE (1 << 15)
#define INF (1 << 16)

using namespace std;

long long int now() {
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
    None,
};

struct Move {
    int from;
    int to;
    int promo;
};

using BB = uint64_t;

struct Position {
    BB colour[2] = {0xFFFFULL, 0xFFFF000000000000ULL};
    BB pieces[6] = {0xFF00000000FF00ULL,
                    0x4200000000000042ULL,
                    0x2400000000000024ULL,
                    0x8100000000000081ULL,
                    0x800000000000008ULL,
                    0x1000000000000010ULL};
    BB ep = 0x0ULL;
    int halfmoves = 0;
    bool castling[4] = {true, true, true, true};
    bool flipped = false;
};

BB flip(BB bb) {
    return __builtin_bswap64(bb);
}

int lsb(BB bb) {
    return __builtin_ctzll(bb);
}

int count(BB bb) {
    return __builtin_popcountll(bb);
}

BB north(BB bb) {
    return bb << 8;
}

BB south(BB bb) {
    return bb >> 8;
}

BB east(BB bb) {
    return (bb << 1) & ~0x0101010101010101ULL;
}

BB west(BB bb) {
    return (bb >> 1) & ~0x8080808080808080ULL;
}

BB nw(BB bb) {
    return (bb << 7) & ~0x8080808080808080ULL;
}

BB ne(BB bb) {
    return (bb << 9) & ~0x0101010101010101ULL;
}

BB sw(BB bb) {
    return (bb >> 9) & ~0x8080808080808080ULL;
}

BB se(BB bb) {
    return (bb >> 7) & ~0x0101010101010101ULL;
}

bool operator==(Move &lhs, Move &rhs) {
    return lhs.from == rhs.from && lhs.to == rhs.to && lhs.promo == rhs.promo;
}

void move_str(Move &move, char *str, bool flip) {
    auto promos = "\0nbrq\0\0";
    str[0] = (move.from % 8) + 'a';
    str[2] = (move.to % 8) + 'a';
    if (flip) {
        str[1] = (7 - (move.from / 8)) + '1';
        str[3] = (7 - (move.to / 8)) + '1';
    } else {
        str[1] = (move.from / 8) + '1';
        str[3] = (move.to / 8) + '1';
    }
    str[4] = promos[move.promo];
    str[5] = '\0';
}

int piece_on(Position &pos, int sq) {
    BB bb = 1ULL << sq;
    for (int i = 0; i < 6; ++i) {
        if (pos.pieces[i] & bb) {
            return i;
        }
    }
    return None;
}

int colour_on(Position &pos, int sq) {
    BB bb = 1ULL << sq;
    if (pos.colour[0] & bb) {
        return 0;
    } else if (pos.pieces[1] & bb) {
        return 1;
    } else {
        return 2;
    }
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
BB ray(int sq, BB blockers, F f) {
    BB mask = f(1ULL << sq);
    for (int i = 1; i < 8; ++i) {
        mask |= f(mask & ~blockers);
    }
    return mask;
}

BB knight(int sq, BB) {
    BB bb = 1ULL << sq;
    return (((bb << 15) | (bb >> 17)) & 0x7F7F7F7F7F7F7F7FULL) | (((bb << 17) | (bb >> 15)) & 0xFEFEFEFEFEFEFEFEULL) |
           (((bb << 10) | (bb >> 6)) & 0xFCFCFCFCFCFCFCFCULL) | (((bb << 6) | (bb >> 10)) & 0x3F3F3F3F3F3F3F3FULL);
}

BB bishop(int sq, BB blockers) {
    return ray(sq, blockers, nw) | ray(sq, blockers, ne) | ray(sq, blockers, sw) | ray(sq, blockers, se);
}

BB rook(int sq, BB blockers) {
    return ray(sq, blockers, north) | ray(sq, blockers, east) | ray(sq, blockers, south) | ray(sq, blockers, west);
}

BB king(int sq, BB) {
    BB bb = 1ULL << sq;
    return (bb << 8) | (bb >> 8) | (((bb >> 1) | (bb >> 9) | (bb << 7)) & 0x7F7F7F7F7F7F7F7FULL) |
           (((bb << 1) | (bb << 9) | (bb >> 7)) & 0xFEFEFEFEFEFEFEFEULL);
}

bool attacked(Position &pos, int sq, bool them = true) {
    BB bb = 1ULL << sq;
    BB kt = pos.colour[them] & pos.pieces[Knight];
    BB BQ = pos.pieces[Bishop] | pos.pieces[Queen];
    BB RQ = pos.pieces[Rook] | pos.pieces[Queen];
    BB pawns = pos.colour[them] & pos.pieces[Pawn];
    BB pawn_attacks = them ? sw(pawns) | se(pawns) : nw(pawns) | ne(pawns);
    return (pawn_attacks & bb) | (kt & knight(sq, 0)) |
           (bishop(sq, pos.colour[0] | pos.colour[1]) & pos.colour[them] & BQ) |
           (rook(sq, pos.colour[0] | pos.colour[1]) & pos.colour[them] & RQ) |
           (king(sq, 0) & pos.colour[them] & pos.pieces[King]);
}

bool makemove(Position &pos, Move &move) {
    int piece = piece_on(pos, move.from);
    int captured = piece_on(pos, move.to);
    pos.colour[0] ^= (1ULL << move.from) | (1ULL << move.to);
    pos.pieces[piece] ^= (1ULL << move.from) | (1ULL << move.to);
    if (piece == Pawn && (1ULL << move.to) == pos.ep) {
        pos.colour[1] ^= (1ULL << (move.to - 8));
        pos.pieces[Pawn] ^= (1ULL << (move.to - 8));
    }
    pos.ep = 0x0ULL;
    if (piece == Pawn && move.to - move.from == 16) {
        pos.ep = (1ULL << (move.from + 8));
    }
    if (captured != None) {
        pos.colour[1] ^= (1ULL << move.to);
        pos.pieces[captured] ^= (1ULL << move.to);
    }
    if (piece == King) {
        BB bb = move.to - move.from == 2 ? 0xa0ULL : move.to - move.from == -2 ? 0x9ULL : 0x0ULL;
        pos.colour[0] ^= bb;
        pos.pieces[Rook] ^= bb;
    }
    if (piece == Pawn && move.to >= 56) {
        pos.pieces[Pawn] ^= (1ULL << move.to);
        pos.pieces[move.promo] ^= (1ULL << move.to);
    }
    BB changed = (1ULL << move.to) | (1ULL << move.from);
    pos.castling[0] &= !(changed & 0x90ULL);
    pos.castling[1] &= !(changed & 0x11ULL);
    pos.castling[2] &= !(changed & 0x9000000000000000ULL);
    pos.castling[3] &= !(changed & 0x1100000000000000ULL);
    flip(pos);
    int ksq = lsb(pos.colour[1] & pos.pieces[King]);
    return !attacked(pos, ksq, false);
}

void add_move(Move *movelist, int &num_moves, int from, int to, int promo) {
    movelist[num_moves] = Move{from, to, promo};
    num_moves++;
}

void generate_pawn_moves(Move *movelist, int &num_moves, BB to_mask, int offset) {
    while (to_mask) {
        int to = lsb(to_mask);
        to_mask &= to_mask - 1;
        if (to >= 56) {
            add_move(movelist, num_moves, to + offset, to, Queen);
            add_move(movelist, num_moves, to + offset, to, Rook);
            add_move(movelist, num_moves, to + offset, to, Bishop);
            add_move(movelist, num_moves, to + offset, to, Knight);
        } else {
            add_move(movelist, num_moves, to + offset, to, None);
        }
    }
}

void generate_piece_moves(Move *movelist, int &num_moves, Position &pos, int piece, BB (*func)(int, BB)) {
    BB copy = pos.colour[0] & pos.pieces[piece];
    while (copy) {
        int fr = lsb(copy);
        copy &= copy - 1;
        BB moves = func(fr, pos.colour[0] | pos.colour[1]);
        moves &= ~pos.colour[0];
        while (moves) {
            int to = lsb(moves);
            moves &= moves - 1;
            add_move(movelist, num_moves, fr, to, None);
        }
    }
}

int movegen(Position &pos, Move *movelist) {
    int num_moves = 0;
    BB all = pos.colour[0] | pos.colour[1];
    BB empty = ~all;
    BB pawns = pos.colour[0] & pos.pieces[Pawn];
    generate_pawn_moves(movelist, num_moves, north(pawns) & empty, -8);
    generate_pawn_moves(movelist, num_moves, north(north(pawns & 0xFF00ULL) & empty) & empty, -16);
    generate_pawn_moves(movelist, num_moves, nw(pawns) & (pos.colour[1] | pos.ep), -7);
    generate_pawn_moves(movelist, num_moves, ne(pawns) & (pos.colour[1] | pos.ep), -9);
    generate_piece_moves(movelist, num_moves, pos, Knight, knight);
    generate_piece_moves(movelist, num_moves, pos, Bishop, bishop);
    generate_piece_moves(movelist, num_moves, pos, Queen, bishop);
    generate_piece_moves(movelist, num_moves, pos, Rook, rook);
    generate_piece_moves(movelist, num_moves, pos, Queen, rook);
    generate_piece_moves(movelist, num_moves, pos, King, king);
    if (pos.castling[0] && !(all & (0x60ULL)) && !attacked(pos, 4) && !attacked(pos, 5)) {
        add_move(movelist, num_moves, 4, 6, None);
    }
    if (pos.castling[1] && !(all & (0xEULL)) && !attacked(pos, 4) && !attacked(pos, 3)) {
        add_move(movelist, num_moves, 4, 2, None);
    }
    return num_moves;
}

int material[] = {100, 339, 372, 582, 1180};
int centralities[] = {2, 20, 16, 1, 3, 11};
int passers[] = {17, 11, 13, 31, 93, 192};
int rook_semi_open = 25;
int rook_open = 35;
int rook_rank78 = 24;

int eval(Position &pos) {
    int score = 10;
    for (int c = 0; c < 2; ++c) {
        BB pawns[2];
        for (int c2 = 0; c2 < 2; ++c2) {
            pawns[c2] = pos.colour[c2] & pos.pieces[Pawn];
        }
        for (int p = 0; p < 6; ++p) {
            BB copy = pos.colour[0] & pos.pieces[p];
            while (copy) {
                int sq = lsb(copy);
                copy &= copy - 1;
                int rank = sq >> 3;
                int file = sq & 7;
                int centrality = (7 - abs(7 - rank - file) - abs(rank - file)) / 2;
                score += centrality * centralities[p];
                if (p == Pawn) {
                    BB bb = 1ULL << sq;
                    BB attack = nw(bb) | ne(bb);
                    for (int i = 0; i < 4; ++i) {
                        attack |= north(attack);
                    }
                    if ((attack & pawns[1]) == 0) {
                        score += passers[rank - 1];
                    }
                } else if (p == Rook) {
                    BB file_bb = 0x101010101010101ULL << file;
                    if ((file_bb & pawns[0]) == 0) {
                        if ((file_bb & pawns[1]) == 0) {
                            score += rook_open;
                        } else {
                            score += rook_semi_open;
                        }
                    }
                    if (rank >= 6) {
                        score += rook_rank78;
                    }
                }
                score += material[p];
            }
        }
        flip(pos);
        score = -score;
    }
    return score;
}

int alphabeta(Position &pos, int alpha, int beta, int depth, int ply, long long int stop_time, Move *pvline) {
    int ksq = lsb(pos.colour[0] & pos.pieces[King]);
    auto in_check = attacked(pos, ksq);
    depth += in_check;
    int static_eval = eval(pos);
    bool in_qsearch = depth <= 0;
    if (in_qsearch) {
        if (static_eval >= beta) {
            return beta;
        }
        if (alpha < static_eval) {
            alpha = static_eval;
        }
    } else if (depth < 3) {
        int margin = 120;
        if (static_eval - margin * depth >= beta) {
            return beta;
        }
    }
    if (now() >= stop_time) {
        return 0;
    }
    Move moves[256];
    int num_moves = movegen(pos, moves);
    int move_scores[256];
    for (int j = 0; j < num_moves; ++j) {
        int move_score = 0;
        if (!in_qsearch && moves[j] == pvline[ply]) {
            move_score = 1 << 16;
        } else {
            int capture = piece_on(pos, moves[j].to);
            if (capture != None) {
                move_score = ((capture + 1) * 8) - piece_on(pos, moves[j].from);
            }
        }
        move_scores[j] = move_score;
    }
    int best_score = -INF;
    for (int i = 0; i < num_moves; ++i) {
        int best_move_score = 0;
        int best_move_score_index = i;
        for (int j = i; j < num_moves; ++j) {
            if (move_scores[j] > best_move_score) {
                best_move_score = move_scores[j];
                best_move_score_index = j;
            }
        }
        auto move = moves[best_move_score_index];
        moves[best_move_score_index] = moves[i];
        move_scores[best_move_score_index] = move_scores[i];
        if (in_qsearch && piece_on(pos, move.to) == None) {
            break;
        }
        auto npos = pos;
        if (!makemove(npos, move)) {
            continue;
        }
        int new_beta = -alpha;
        int new_alpha = -alpha - 1;
        goto do_search;
    full_search:
        new_alpha = -beta;
    do_search:
        int score = -alphabeta(npos, new_alpha, new_beta, depth - 1, ply + 1, stop_time, pvline);
        if (score > alpha && new_alpha != -beta) {
            goto full_search;
        }
        if (score > best_score) {
            best_score = score;
            if (score > alpha) {
                alpha = score;
                pvline[ply] = move;
            }
        }
        if (alpha >= beta) {
            break;
        }
    }
    if (!in_qsearch && best_score == -INF) {
        if (in_check) {
            return -MATE_SCORE;
        } else {
            return 0;
        }
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
            auto stop_time = now() + (pos.flipped ? btime : wtime) / 30;
            char bestmove_str[] = "bestmove       ";
            Move pvline[128];
            for (int i = 1; i < 128; ++i) {
                alphabeta(pos, -INF, INF, i, 0, stop_time, pvline);
                if (now() >= stop_time) {
                    break;
                }
                move_str(pvline[0], &bestmove_str[9], pos.flipped);
            }
            puts(bestmove_str);
        } else if (word == "position") {
            pos = Position();
        } else {
            int num_moves = movegen(pos, moves);
            for (int i = 0; i < num_moves; ++i) {
                char movestr[6];
                move_str(moves[i], movestr, pos.flipped);
                if (movestr == word) {
                    makemove(pos, moves[i]);
                    break;
                }
            }
        }
    }
}
