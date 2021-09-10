#include <stdio.h>
#include <algorithm>
#include <cstdint>
#include <cstring>
#include <ctime>

#define MATE_SCORE (1 << 15)
#define INF (1 << 16)

#ifdef USE_SEARCHINFO
extern unsigned long long int nodes_searched;
#endif

long long int now() {
    timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec * 1000 + t.tv_nsec / 1000000;
}

namespace chess {

enum class Piece : std::uint8_t
{
    Pawn = 0,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
    None,
};

// clang-format off
enum Square
{
    a1=0, b1, c1, d1, e1, f1, g1, h1,
    a2, b2, c2, d2, e2, f2, g2, h2,
    a3, b3, c3, d3, e3, f3, g3, h3,
    a4, b4, c4, d4, e4, f4, g4, h4,
    a5, b5, c5, d5, e5, f5, g5, h5,
    a6, b6, c6, d6, e6, f6, g6, h6,
    a7, b7, c7, d7, e7, f7, g7, h7,
    a8, b8, c8, d8, e8, f8, g8, h8,
};
// clang-format on

struct Move {
    int from;
    int to;
    Piece promo;
};

using Bitboard = std::uint64_t;

struct Position {
    Bitboard colour[2] = {0xFFFFULL, 0xFFFF000000000000ULL};
    Bitboard pieces[6] = {0xFF00000000FF00ULL,
                          0x4200000000000042ULL,
                          0x2400000000000024ULL,
                          0x8100000000000081ULL,
                          0x800000000000008ULL,
                          0x1000000000000010ULL};
    Bitboard ep = 0x0ULL;
    int halfmoves = 0;
    bool castling[4] = {true, true, true, true};
    bool flipped = false;
};

[[nodiscard]] constexpr Bitboard flip(Bitboard bb) noexcept {
    return __builtin_bswap64(bb);
}

[[nodiscard]] constexpr int lsbll(Bitboard bb) noexcept {
    return __builtin_ctzll(bb);
}

[[nodiscard]] constexpr auto count(Bitboard bb) noexcept {
    return __builtin_popcountll(bb);
}

[[nodiscard]] constexpr Bitboard north(Bitboard bb) noexcept {
    return bb << 8;
}

[[nodiscard]] constexpr Bitboard south(Bitboard bb) noexcept {
    return bb >> 8;
}

[[nodiscard]] constexpr Bitboard east(Bitboard bb) noexcept {
    return (bb << 1) & ~0x0101010101010101ULL;
}

[[nodiscard]] constexpr Bitboard west(Bitboard bb) noexcept {
    return (bb >> 1) & ~0x8080808080808080ULL;
}

[[nodiscard]] constexpr Bitboard nw(Bitboard bb) noexcept {
    return (bb << 7) & ~0x8080808080808080ULL;
}

[[nodiscard]] constexpr Bitboard ne(Bitboard bb) noexcept {
    return (bb << 9) & ~0x0101010101010101ULL;
}

[[nodiscard]] constexpr Bitboard sw(Bitboard bb) noexcept {
    return (bb >> 9) & ~0x8080808080808080ULL;
}

[[nodiscard]] constexpr Bitboard se(Bitboard bb) noexcept {
    return (bb >> 7) & ~0x0101010101010101ULL;
}

[[nodiscard]] constexpr Bitboard adjacent(Bitboard bb) noexcept {
    return (bb << 8) | (bb >> 8) | (((bb >> 1) | (bb >> 9) | (bb << 7)) & 0x7F7F7F7F7F7F7F7FULL) |
           (((bb << 1) | (bb << 9) | (bb >> 7)) & 0xFEFEFEFEFEFEFEFEULL);
}

[[nodiscard]] constexpr Bitboard knight(Bitboard bb) noexcept {
    return (((bb << 15) | (bb >> 17)) & 0x7F7F7F7F7F7F7F7FULL) | (((bb << 17) | (bb >> 15)) & 0xFEFEFEFEFEFEFEFEULL) |
           (((bb << 10) | (bb >> 6)) & 0xFCFCFCFCFCFCFCFCULL) | (((bb << 6) | (bb >> 10)) & 0x3F3F3F3F3F3F3F3FULL);
}

[[maybe_unused]] static bool operator==(const Move &lhs, const Move &rhs) {
    return lhs.from == rhs.from && lhs.to == rhs.to && lhs.promo == rhs.promo;
}

[[maybe_unused]] static void move_str(const Move &move, char *str, const bool flip) {
    static constexpr char promos[] = {'\0', 'n', 'b', 'r', 'q', '\0', '\0'};

    str[0] = static_cast<char>((move.from % 8) + 'a');
    str[2] = static_cast<char>((move.to % 8) + 'a');
    if (flip) {
        str[1] = static_cast<char>(7 - (move.from / 8) + '1');
        str[3] = static_cast<char>(7 - (move.to / 8) + '1');
    } else {
        str[1] = static_cast<char>((move.from / 8) + '1');
        str[3] = static_cast<char>((move.to / 8) + '1');
    }

    str[4] = promos[static_cast<int>(move.promo)];
    str[5] = '\0';
}

static Piece piece_on(const Position &pos, const int sq) noexcept {
    const auto bb = Bitboard(1ULL << sq);
    for (int i = 0; i < 6; ++i) {
        if (pos.pieces[i] & bb) {
            return static_cast<Piece>(i);
        }
    }
    return Piece::None;
}

static int colour_on(const Position &pos, const int sq) {
    const auto bb = Bitboard(1ULL << sq);
    if (pos.colour[0] & bb) {
        return 0;
    } else if (pos.pieces[1] & bb) {
        return 1;
    } else {
        return 2;
    }
}

[[maybe_unused]] static void flip(Position &pos) noexcept {
    pos.colour[0] = flip(pos.colour[0]);
    pos.colour[1] = flip(pos.colour[1]);

    for (int i = 0; i < 6; ++i) {
        pos.pieces[i] = flip(pos.pieces[i]);
    }

    pos.ep = flip(pos.ep);

    {
        const auto c = pos.colour[0];
        pos.colour[0] = pos.colour[1];
        pos.colour[1] = c;
    }
    {
        const auto c = pos.castling[0];
        pos.castling[0] = pos.castling[2];
        pos.castling[2] = c;
    }
    {
        const auto c = pos.castling[1];
        pos.castling[1] = pos.castling[3];
        pos.castling[3] = c;
    }
    pos.flipped = !pos.flipped;
}

namespace raycast {

constexpr Bitboard N(const int sq, const Bitboard blockers) {
    auto mask = north(Bitboard(1ULL << sq));
    for (int i = 1; i < 8; ++i) {
        mask |= north(mask & ~blockers);
    }
    return mask;
}

constexpr Bitboard S(const int sq, const Bitboard blockers) {
    auto mask = south(Bitboard(1ULL << sq));
    for (int i = 1; i < 8; ++i) {
        mask |= south(mask & ~blockers);
    }
    return mask;
}

constexpr Bitboard E(const int sq, const Bitboard blockers) {
    auto mask = east(Bitboard(1ULL << sq));
    for (int i = 1; i < 8; ++i) {
        mask |= east(mask & ~blockers);
    }
    return mask;
}

constexpr Bitboard W(const int sq, const Bitboard blockers) {
    auto mask = west(Bitboard(1ULL << sq));
    for (int i = 1; i < 8; ++i) {
        mask |= west(mask & ~blockers);
    }
    return mask;
}

constexpr Bitboard NW(const int sq, const Bitboard blockers) {
    auto mask = nw(Bitboard(1ULL << sq));
    for (int i = 1; i < 8; ++i) {
        mask |= nw(mask & ~blockers);
    }
    return mask;
}

constexpr Bitboard NE(const int sq, const Bitboard blockers) {
    auto mask = ne(Bitboard(1ULL << sq));
    for (int i = 1; i < 8; ++i) {
        mask |= ne(mask & ~blockers);
    }
    return mask;
}

constexpr Bitboard SW(const int sq, const Bitboard blockers) {
    auto mask = sw(Bitboard(1ULL << sq));
    for (int i = 1; i < 8; ++i) {
        mask |= sw(mask & ~blockers);
    }
    return mask;
}

constexpr Bitboard SE(const int sq, const Bitboard blockers) {
    auto mask = se(Bitboard(1ULL << sq));
    for (int i = 1; i < 8; ++i) {
        mask |= se(mask & ~blockers);
    }
    return mask;
}

constexpr Bitboard knight(const int sq, const Bitboard) {
    return chess::knight(Bitboard(1ULL << sq));
}

constexpr Bitboard bishop(const int sq, const Bitboard blockers) {
    return NW(sq, blockers) | NE(sq, blockers) | SW(sq, blockers) | SE(sq, blockers);
}

constexpr Bitboard rook(const int sq, const Bitboard blockers) {
    return N(sq, blockers) | E(sq, blockers) | S(sq, blockers) | W(sq, blockers);
}

constexpr Bitboard king(const int sq, const Bitboard) {
    return adjacent(Bitboard(1ULL << sq));
}

}  // namespace raycast

[[nodiscard]] bool attacked(const Position &pos, const int sq, const bool them) {
    const auto bb = Bitboard(1ULL << sq);
    const auto kt = pos.colour[them] & pos.pieces[static_cast<int>(Piece::Knight)];
    const auto BQ = pos.pieces[static_cast<int>(Piece::Bishop)] | pos.pieces[static_cast<int>(Piece::Queen)];
    const auto RQ = pos.pieces[static_cast<int>(Piece::Rook)] | pos.pieces[static_cast<int>(Piece::Queen)];
    const auto pawns = pos.colour[them] & pos.pieces[static_cast<int>(Piece::Pawn)];
    const auto pawn_attacks = them ? sw(pawns) | se(pawns) : nw(pawns) | ne(pawns);

    return
        // Pawns
        (pawn_attacks & bb) |
        // Knights
        (bb & knight(kt)) |
        // Bishops - Queens
        (raycast::bishop(sq, pos.colour[0] | pos.colour[1]) & pos.colour[them] & BQ) |
        // Rooks - Queens
        (raycast::rook(sq, pos.colour[0] | pos.colour[1]) & pos.colour[them] & RQ) |
        // King
        (adjacent(bb) & pos.colour[them] & pos.pieces[static_cast<int>(Piece::King)]);
}

bool makemove(Position &pos, const Move &move) {
    const auto piece = piece_on(pos, move.from);
    const auto captured = piece_on(pos, move.to);

    // Move our piece
    pos.colour[0] ^= Bitboard(1ULL << move.from) | Bitboard(1ULL << move.to);
    pos.pieces[static_cast<int>(piece)] ^= Bitboard(1ULL << move.from) | Bitboard(1ULL << move.to);

    // En passant
    if (piece == Piece::Pawn && Bitboard(1ULL << move.to) == pos.ep) {
        // Remove their pawn
        pos.colour[1] ^= Bitboard(1ULL << (move.to - 8));
        pos.pieces[static_cast<int>(Piece::Pawn)] ^= Bitboard(1ULL << (move.to - 8));
    }

    pos.ep = 0x0ULL;

    // Double
    if (piece == Piece::Pawn && move.to - move.from == 16) {
        pos.ep = Bitboard(1ULL << (move.from + 8));
    }

    // Remove their piece
    if (captured != Piece::None) {
        pos.colour[1] ^= Bitboard(1ULL << move.to);
        pos.pieces[static_cast<int>(captured)] ^= Bitboard(1ULL << move.to);
    }

    // Castling
    if (piece == Piece::King) {
        const auto bb = move.to - move.from == 2 ? 0xa0ULL : move.to - move.from == -2 ? 0x9ULL : 0x0ULL;
        pos.colour[0] ^= bb;
        pos.pieces[static_cast<int>(Piece::Rook)] ^= bb;
    }

    // Promo
    if (piece == Piece::Pawn && move.to >= Square::a8) {
        // Replace pawn with new piece
        pos.pieces[static_cast<int>(Piece::Pawn)] ^= Bitboard(1ULL << move.to);
        pos.pieces[static_cast<int>(move.promo)] ^= Bitboard(1ULL << move.to);
    }

    // Remove castling permissions
    const auto changed = (1ULL << move.to) | (1ULL << move.from);
    pos.castling[0] &= !(changed & 0x90ULL);
    pos.castling[1] &= !(changed & 0x11ULL);
    pos.castling[2] &= !(changed & 0x9000000000000000ULL);
    pos.castling[3] &= !(changed & 0x1100000000000000ULL);

    // Flip the board
    flip(pos);

    // Did this move hang our king?
    const int ksq = lsbll(pos.colour[1] & pos.pieces[static_cast<int>(chess::Piece::King)]);
    return !attacked(pos, ksq, false);
}

void add_move(chess::Move *movelist, int &num_moves, const int from, const int to, const chess::Piece promo) {
    movelist[num_moves] = Move{from, to, promo};
    num_moves++;
}

void generate_pawn_moves(Move *movelist, int &num_moves, Bitboard to_mask, const int offset) {
    while (to_mask) {
        auto to = lsbll(to_mask);
        to_mask &= to_mask - 1;

        // Promotion
        if (to >= Square::a8) {
            add_move(movelist, num_moves, to + offset, to, Piece::Queen);
            add_move(movelist, num_moves, to + offset, to, Piece::Rook);
            add_move(movelist, num_moves, to + offset, to, Piece::Bishop);
            add_move(movelist, num_moves, to + offset, to, Piece::Knight);
        } else {
            add_move(movelist, num_moves, to + offset, to, Piece::None);
        }
    }
}

void generate_piece_moves(Move *movelist,
                          int &num_moves,
                          const Position &pos,
                          const Piece piece,
                          Bitboard (*func)(int, Bitboard)) {
    auto copy = pos.colour[0] & pos.pieces[static_cast<int>(piece)];
    while (copy) {
        auto fr = lsbll(copy);
        copy &= copy - 1;

        auto moves = func(fr, pos.colour[0] | pos.colour[1]);
        moves &= ~pos.colour[0];

        while (moves) {
            auto to = lsbll(moves);
            moves &= moves - 1;

            add_move(movelist, num_moves, fr, to, Piece::None);
        }
    }
}

int movegen(const Position &pos, Move *movelist) {
    int num_moves = 0;

    const auto all = pos.colour[0] | pos.colour[1];
    const auto empty = ~all;
    const auto pawns = pos.colour[0] & pos.pieces[static_cast<int>(Piece::Pawn)];

    // Pawns
    generate_pawn_moves(movelist, num_moves, north(pawns) & empty, -8);
    generate_pawn_moves(movelist, num_moves, north(north(pawns & Bitboard(0xFF00ULL)) & empty) & empty, -16);
    generate_pawn_moves(movelist, num_moves, nw(pawns) & (pos.colour[1] | pos.ep), -7);
    generate_pawn_moves(movelist, num_moves, ne(pawns) & (pos.colour[1] | pos.ep), -9);
    // Others
    generate_piece_moves(movelist, num_moves, pos, Piece::Knight, raycast::knight);
    generate_piece_moves(movelist, num_moves, pos, Piece::Bishop, raycast::bishop);
    generate_piece_moves(movelist, num_moves, pos, Piece::Queen, raycast::bishop);
    generate_piece_moves(movelist, num_moves, pos, Piece::Rook, raycast::rook);
    generate_piece_moves(movelist, num_moves, pos, Piece::Queen, raycast::rook);
    generate_piece_moves(movelist, num_moves, pos, Piece::King, raycast::king);

    // Castling - King side
    if (pos.castling[0] && !(all & Bitboard(0x60ULL)) && !attacked(pos, Square::e1, true) &&
        !attacked(pos, Square::f1, true)) {
        add_move(movelist, num_moves, Square::e1, Square::g1, Piece::None);
    }

    // Castling - Queen side
    if (pos.castling[1] && !(all & Bitboard(0xEULL)) && !attacked(pos, Square::e1, true) &&
        !attacked(pos, Square::d1, true)) {
        add_move(movelist, num_moves, Square::e1, Square::c1, Piece::None);
    }

    return num_moves;
}

}  // namespace chess

namespace search {

constexpr int material[] = {100, 339, 372, 582, 1180};
constexpr int centralities[] = {2, 20, 16, 1, 3, 11};
constexpr int passers[] = {17, 11, 13, 31, 93, 192};
constexpr int rook_semi_open = 25;
constexpr int rook_open = 35;
constexpr int rook_rank78 = 24;

#ifdef USE_SEARCHINFO
unsigned long long int nodes_searched;
#endif

[[nodiscard]] int eval(chess::Position &pos) {
    // Tempo bonus
    int score = 10;

    for (int c = 0; c < 2; ++c) {
        chess::Bitboard pawns[2];
        for (int c2 = 0; c2 < 2; ++c2) {
            pawns[c2] = pos.colour[c2] & pos.pieces[static_cast<int>(chess::Piece::Pawn)];
        }

        for (int p = 0; p < 6; ++p) {
            auto copy = pos.colour[0] & pos.pieces[p];
            while (copy) {
                const auto sq = chess::lsbll(copy);
                copy &= copy - 1;

                const int rank = sq >> 3;
                const int file = sq & 7;

                // Centrality
                const int centrality = (7 - std::abs(7 - rank - file) - std::abs(rank - file)) / 2;
                score += centrality * centralities[p];

                // Pawn eval
                if (p == static_cast<int>(chess::Piece::Pawn)) {
                    const auto bb = 1ULL << sq;

                    // Passed pawns
                    auto attack = chess::nw(bb) | chess::ne(bb);
                    for (auto i = 0; i < 4; ++i) {
                        attack |= chess::north(attack);
                    }
                    const auto is_passed = (attack & pawns[1]) == 0;
                    if (is_passed) {
                        score += passers[rank - 1];
                    }
                } else if (p == static_cast<int>(chess::Piece::Rook)) {
                    // Open and semi-open files
                    const auto file_bb = 0x101010101010101ULL << file;
                    if ((file_bb & pawns[0]) == 0) {
                        if ((file_bb & pawns[1]) == 0) {
                            score += rook_open;
                        } else {
                            score += rook_semi_open;
                        }
                    }

                    // Bonus on 7th/8th rank
                    if (rank >= 6) {
                        score += rook_rank78;
                    }
                }

                // Material
                score += material[p];
            }
        }

        chess::flip(pos);
        score = -score;
    }

    return score;
}

int alphabeta(chess::Position &pos,
              int alpha,
              const int beta,
              int depth,
              const int ply,
              const long long int stop_time,
              chess::Move *pvline) {
    const int ksq = chess::lsbll(pos.colour[0] & pos.pieces[static_cast<int>(chess::Piece::King)]);
    const auto in_check = chess::attacked(pos, ksq, true);

    // In-check extension
    if (in_check) {
        depth++;
    }

    const int static_eval = eval(pos);
    const bool in_qsearch = depth <= 0;
    if (in_qsearch) {
        if (static_eval >= beta) {
            return beta;
        }

        if (alpha < static_eval) {
            alpha = static_eval;
        }
    } else if (depth < 3) {
        // Reverse futility pruning
        const int margin = 120;
        if (static_eval - margin * depth >= beta) {
            return beta;
        }
    }

    // Did we run out of time?
    if (now() >= stop_time) {
        return 0;
    }

    chess::Move moves[256];
    const int num_moves = chess::movegen(pos, moves);

    int move_scores[256];
    for (int j = 0; j < num_moves; ++j) {
        auto move_score = 0;

        // PV-move first
        if (!in_qsearch && moves[j] == pvline[ply]) {
            move_score = 1 << 16;
        } else {
            // MVVLVA
            const auto capture = chess::piece_on(pos, moves[j].to);
            if (capture != chess::Piece::None) {
                move_score =
                    ((static_cast<int>(capture) + 1) * 8) - static_cast<int>(chess::piece_on(pos, moves[j].from));
            }
        }
        move_scores[j] = move_score;
    }

    int best_score = -INF;
    for (int i = 0; i < num_moves; ++i) {
        // Pick next move
        int best_move_score = 0;
        int best_move_score_index = i;
        for (int j = i; j < num_moves; ++j) {
            if (move_scores[j] > best_move_score) {
                best_move_score = move_scores[j];
                best_move_score_index = j;
            }
        }

        const auto move = moves[best_move_score_index];
        moves[best_move_score_index] = moves[i];
        move_scores[best_move_score_index] = move_scores[i];

        // Since moves are ordered captures first, break in qsearch
        if (in_qsearch && chess::piece_on(pos, move.to) == chess::Piece::None) {
            break;
        }

        auto npos = pos;

        // Check move legality
        if (!chess::makemove(npos, move)) {
            continue;
        }

#ifdef USE_SEARCHINFO
        nodes_searched++;
#endif

        // Poor man's PVS
        const int new_beta = -alpha;
        int new_alpha = -alpha - 1;
        goto do_search;
    full_search:
        new_alpha = -beta;
    do_search:
        const int score = -alphabeta(npos, new_alpha, new_beta, depth - 1, ply + 1, stop_time, pvline);
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

    // No legal moves
    if (!in_qsearch && best_score == -INF) {
        // Checkmate
        if (in_check) {
            return -MATE_SCORE;
        }
        // Stalemate
        else {
            return 0;
        }
    }

    return alpha;
}

}  // namespace search

namespace uci {

void go(chess::Position &pos, const int time) {
    const auto stop_time = now() + time / 30;
    char bestmove_str[] = "bestmove       ";
    chess::Move pvline[128];
#ifdef USE_SEARCHINFO
    search::nodes_searched = 0;
    const auto start_time = now();
#endif
    // Iterative deepening
    for (int i = 1; i < 128; ++i) {
#ifdef USE_SEARCHINFO
        const auto score = search::alphabeta(pos, -INF, INF, i, 0, stop_time, pvline);
#else
        search::alphabeta(pos, -INF, INF, i, 0, stop_time, pvline);
#endif

        // Did we run out of time?
        if (now() >= stop_time) {
            break;
        }

        chess::move_str(pvline[0], &bestmove_str[9], pos.flipped);
#ifdef USE_SEARCHINFO
        auto elapsed = now() - start_time;
        if (elapsed == 0) {
            elapsed = 1;
        }
        const auto nps = search::nodes_searched * 1000 / elapsed;
        std::printf(
            "info depth %i score %i nodes %lld nps %lld time %lld\n", i, score, search::nodes_searched, nps, elapsed);
#endif
    }

    puts(bestmove_str);
}

void next(char *ptr) {
    char c;
    while ((c = getchar())) {
        if (c == '\n' || c == '\0' || c == ' ') {
            break;
        }
        *ptr = c;
        ptr++;
    }
    *ptr = '\0';
}

void listen() {
    char word[4096];
    auto pos = chess::Position();
    chess::Move moves[256];

    // Pretend to wait for "uci"
    getchar();

    // Tell the GUI about us
    puts("id name 4ku2\nid author kz04px\nuciok");

    while (true) {
        // Get next word
        next(word);

        // quit
        if (word[0] == 'q') {
            break;
        }
        // isready
        else if (word[0] == 'i' && word[1] == 's') {
            puts("readyok");
        }
        // go
        else if (word[0] == 'g' && word[1] == 'o') {
            // wtime
            next(word);
            next(word);
            const int wtime = atoi(word);

            // btime
            next(word);
            next(word);
            const int btime = atoi(word);

            go(pos, pos.flipped ? btime : wtime);
        }
        // position
        else if (word[0] == 'p' && word[1] == 'o') {
            pos = chess::Position();
        }
        // Try parse a move
        else {
            const int num_moves = chess::movegen(pos, moves);
            for (int i = 0; i < num_moves; ++i) {
                char movestr[6];
                chess::move_str(moves[i], movestr, pos.flipped);
                if (strncmp(movestr, word, strlen(movestr)) == 0) {
                    chess::makemove(pos, moves[i]);
                    break;
                }
            }
        }
    }
}

}  // namespace uci

int main() {
    setbuf(stdout, NULL);
    uci::listen();
    return 0;
}
