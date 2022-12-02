import argparse
import os.path
import re

def get_args():
    parser = argparse.ArgumentParser(description='C++ minifier')
    parser.add_argument('src', help='Path to source file')
    return parser.parse_args()

def get_keywords(src):
    keywords = set({"int", "short", "char", "auto", "bool", "void", "using", "namespace", "define", "else",
                    "long", "unsigned", "timespec", "struct", "class", "return", "operator", "typename", "const", "string", "goto",
                    "uint64_t", "int64_t", "uint32_t", "int32_t", "uint16_t", "int16_t", "uint8_t", "int8_t"})

    get_next = False

    for token in re.split("([^a-zA-Z0-9])", src):
        if token in [" ", "\t", "\n", "nodiscard"]:
            continue

        if get_next and token.isalnum():
            keywords.add(token)
            get_next = False

        # Declared types
        if token in ["struct", "class", "using", "typename"]:
            get_next = True

    return keywords

def get_source(path):
    return open(path, "r").read()

def get_tokens(src):
    return re.split("([^a-zA-Z0-9])", src)

def filter_empty(tokens):
    return [n for n in tokens if n]

def filter_delete(tokens):
    new = []
    in_comment = False
    chunk = []
    skip = False

    for i in range(len(tokens)):
        prev = tokens[i-1] if i > 0 else None
        curr = tokens[i]
        next = tokens[i+1] if i+1 < len(tokens) else None

        if curr == "/" and next == "/":
            in_comment = True
        elif curr == "\n":
            in_comment = False
            if chunk == ['/', '/', ' ', 'minify', ' ', 'delete', ' ', 'on']:
                skip = True
            elif chunk == ['/', '/', ' ', 'minify', ' ', 'delete', ' ', 'off']:
                skip = False
            chunk = []

        if in_comment:
            chunk.append(curr)
        elif not skip:
            new.append(curr)

    return new

def filter_comments(tokens):
    new = []
    in_comment = False

    for i in range(len(tokens)):
        prev = tokens[i-1] if i > 0 else None
        curr = tokens[i]
        next = tokens[i+1] if i+1 < len(tokens) else None

        if curr == "/" and next == "/":
            in_comment = True
        elif curr == "\n":
            in_comment = False

        if not in_comment:
            new.append(curr)

    return new

def filter_block_comments(tokens):
    new = []
    in_block_comment = False

    for i in range(len(tokens)):
        prev = tokens[i-1] if i > 0 else None
        curr = tokens[i]
        next = tokens[i+1] if i+1 < len(tokens) else None

        if curr == "/" and next == "*":
            in_block_comment = True
        elif prev == "*" and curr == "/":
            in_block_comment = False
            continue

        if not in_block_comment:
            new.append(curr)

    return new

def filter_newlines(tokens):
    new = []
    in_preprocessor = False

    for i in range(len(tokens)):
        prev = tokens[i-1] if i > 0 else None
        curr = tokens[i]
        next = tokens[i+1] if i+1 < len(tokens) else None

        is_first = prev == None or prev == "\n"
        in_preprocessor = in_preprocessor or (is_first and curr == "#")

        if curr == "\n" and not in_preprocessor:
            in_preprocessor = False
        else:
            new.append(curr)

        if curr == "\n":
            in_preprocessor = False

    return new

def collect_strings(tokens):
    new = []
    chunk = []
    in_string = False

    for token in tokens:
        string_start = not in_string and token == "\""
        string_end = in_string and token == "\""

        if string_start:
            in_string = True
            chunk = []
        elif string_end:
            in_string = False
            new.append("\"" + "".join(chunk) + "\"")
        elif in_string:
            chunk.append(token)
        else:
            new.append(token)

    return new

def collect_underscores(tokens):
    new = []
    prev = None
    attach = False

    for token in tokens:
        if token == "_":
            attach = True
        elif not token.isalnum():# or token in [" ", ";", "\n", "(", ")", "{", "}", "[", "]"]:
            attach = False

        if attach and new and prev != " ":
            new[len(new)-1] += token
        else:
            new.append(token)

        prev = token

    return new

def collect_tags(tokens):
    new = []
    chunk = []
    in_tag = False

    i = 0
    while i < len(tokens):
        prev = tokens[i-1] if i > 0 else None
        curr = tokens[i]
        next = tokens[i+1] if i+1 < len(tokens) else None

        tag_start = curr == "[" and next == "["
        tag_end = curr == "]" and next == "]"

        if tag_start:
            in_tag = True
            chunk = []
            i += 1
        elif tag_end:
            in_tag = False
            new.append("[[" + "".join(chunk) + "]]")
            i += 1
        elif in_tag:
            chunk.append(curr)
        else:
            new.append(curr)

        i += 1

    return new

def collect_scope(tokens):
    new = []
    attach = False

    i = 0
    while i < len(tokens):
        prev = tokens[i-1] if i > 0 else None
        curr = tokens[i]
        next = tokens[i+1] if i+1 < len(tokens) else None

        if curr == ":" and next == ":":
            attach = True
        elif curr in [" ", "\t", "\n", "(", ";"]:
            attach = False

        if attach and new and prev != " ":
            new[len(new)-1] += curr
        else:
            new.append(curr)

        i += 1

    return new

def filter_whitespace(tokens, keywords):
    new = []
    magic = {"=", ";", ",", "&", "!", "-", "+", "*", "{", "(", ")", "<", ">"}

    prev = None

    for token in tokens:
        if token in [" ", "\t"]:
            continue

        if prev in keywords and token not in magic:
            new.append(" ")

        new.append(token)

        if prev == "define":
            new.append(" ")

        prev = token

    return new

def filter_const(tokens):
    new = []

    for token in tokens:
        if token == "const":
            pass
        else:
            new.append(token)

    return new

def filter_tags(tokens):
    tags = ["[[nodiscard]]", "[[maybe_unused]]", "noexcept"]
    new = []

    for token in tokens:
        if token in tags:
            pass
        else:
            new.append(token)

    return new

def rename(tokens):
    replacements = dict({
        # Types
        "Position":"a",
        "Move":"b",
        "Stack":"cc",
        # Variable names
        "pos":"ac",
        "move":"d",
        "moves":"e",
        "colour":"f",
        "piece":"g",
        "pieces":"h",
        "halfmoves":"j",
        "castling":"k",
        "flipped":"l",
        "values":"m",
        "from":"n",
        "to":"o",
        "str":"p",
        "mask":"q",
        "promo":"r",
        "promos":"s",
        "blockers":"t",
        "Pawn":"u",
        "Knight":"v",
        "Bishop":"w",
        "Rook":"x",
        "Queen":"y",
        "King":"z",
        "None":"A",
        "flip":"B",
        "material":"C",
        "centralities":"D",
        "passers":"E",
        "rook_semi_open":"F",
        "rook_open":"G",
        "rook_rank78":"H",
        "lhs":"J",
        "rhs":"K",
        "copy":"L",
        "movelist":"M",
        "empty":"N",
        "attack":"ab",
        "score":"ae",
        "them":"ah",
        "pawns":"ai",
        "pawn_attacks":"aj",
        "captured":"ak",
        "num_moves":"al",
        "to_mask":"am",
        "offset":"an",
        "centrality":"ar",
        "rank":"as",
        "file":"at",
        "file_bb":"au",
        "alpha":"av",
        "beta":"aw",
        "depth":"ax",
        "ply":"ay",
        "stop_time":"az",
        "in_check":"ba",
        "static_eval":"bb",
        "margin":"bc",
        "npos":"bd",
        "movestr":"be",
        "word":"bf",
        "bestmove_str":"bg",
        "stack":"bh",
        "new_alpha":"bi",
        "new_beta":"bj",
        "move_scores":"bm",
        "move_score":"bn",
        "in_qsearch":"bo",
        "capture":"bp",
        "best_score":"bq",
        "best_move_score":"br",
        "best_move_score_index":"bs",
        "wtime":"bt",
        "btime":"bu",
        "func":"bv",
        "best_move_index":"cb",
        "killer":"cd",
        "hash_history":"ce",
        "old_hash":"cf",
        "phase":"cg",
        "phases":"ch",
        "do_null":"ci",
        "full_window":"cj",
        "moves_evaluated":"ck",
        "keys":"cl",
        "transposition_table":"cm",
        "tt_key":"cn",
        "tt_entry":"co",
        "tt_move":"cp",
        "flag":"cq",
        "tt_flag":"cr",
        "TT_Entry":"cs",
        "MAX_TT_SIZE":"ct",
        "best_move":"cu",
        "bishop_pair":"cv",
        "pawn_doubled":"cw",
        "pawn_passed_blocked":"cx",
        "outside_files":"cy",
        "pawn_protection":"cz",
        "protected_by_pawns":"da",
        "piece_bb":"db",
        "hh_table":"dc",
        "thread_count": "de",
        "threads": "df",
        "shield": "dg",
        "king_shield": "dh",
        "key":"di",
        "start_time":"dj",
        "allocated_time":"dk",
        "stops":"dl",
        "stop":"dm",
        # Labels
        "do_search":"bk",
        "full_search":"bl",
        # Function names
        "knight":"O",
        "bishop":"P",
        "rook":"Q",
        "king":"R",
        "makemove":"S",
        "attacked":"T",
        "add_move":"U",
        "generate_pawn_moves":"V",
        "generate_piece_moves":"W",
        "movegen":"X",
        "get_hash":"Y",
        "piece_on":"Z",
        "move_str":"aa",
        "alphabeta":"ad",
        "north":"ao",
        "south":"ap",
        "eval":"aq",
        "count":"bw",
        "lsb":"bx",
        "east":"by",
        "west":"bz",
        "now":"ca",
        "iteratively_deepen": "dd",
        "ray":"dj",
        # Macros
        "MATE_SCORE":"af",
        "INF":"ag",
        # Constants
        "true":"1",
        "false":"0",
    })
    new = []

    for token in tokens:
        if token in replacements:
            new.append(replacements[token])
        else:
            new.append(token)

    return new

def main():
    args = get_args()

    if not os.path.isfile(args.src):
        raise Exception("File not found")

    src = get_source(args.src)

    keywords = get_keywords(src)

    src = get_tokens(src)
    src = filter_empty(src)
    src = filter_delete(src)
    src = filter_comments(src)
    src = filter_block_comments(src)
    src = filter_newlines(src)
    src = filter_const(src)
    src = collect_strings(src)
    src = collect_underscores(src)
    src = collect_tags(src)
    src = collect_scope(src)
    src = filter_whitespace(src, keywords)
    src = filter_tags(src)
    src = rename(src)

    for word in src:
        print(word, end="")

if __name__ == "__main__":
    main()
