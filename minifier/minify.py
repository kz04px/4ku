import argparse
import re
import os.path
from dataclasses import dataclass
import time


@dataclass
class Settings:
    rename: bool
    filters: set
    deletions: set
    replacements: dict

    def __init__(self):
        self.rename = False
        self.filters = set(
            [
                "whitespace",
                "line_comment",
                "block_comment",
                "const",
                "nodiscard",
                "maybe_unused",
                "noexcept",
                "semicolon",
                "trailing_whitespace",
                "trailing_newline",
            ]
        )
        self.deletions = set(["assert", "static_assert"])
        self.replacements = dict({"CLOCK_MONOTONIC": str(time.CLOCK_MONOTONIC)})


def name_generator() -> str:
    def get_char(idx: int) -> str:
        if idx % 52 < 26:
            return str(chr(ord("a") + idx % 26))
        else:
            return str(chr(ord("A") + idx % 26))

    def get_string(idx: int) -> str:
        if idx < 52:
            return get_char(idx)
        else:
            return get_char(idx // 52 - 1) + get_char(idx % 52)

    idx = 0
    while True:
        ans = get_string(idx)
        yield ans
        idx += 1


def recurse_rename(tokens: list, rename: set, replacements: dict, i: int = 0):
    eat = 0
    found_enum = False
    bracket_depth = 0

    # Create generator at the right point
    gen = name_generator()

    while i < len(tokens):
        eat += 1
        if tokens[i] in ["enum", "struct", "class"]:
            found_enum = True
            i += 1
        elif tokens[i] in rename or tokens[i] in replacements:
            # Create replacement word
            if tokens[i] not in replacements:
                # Find an unused replacement
                w = next(gen)
                while w in replacements.values():
                    w = next(gen)

                # Store new replacement
                replacements[tokens[i]] = w

            # Replace
            tokens[i] = replacements[tokens[i]]

            i += 1
        elif tokens[i] == "{":
            bracket_depth += 1
            i += 1
            if not found_enum:
                gg = recurse_rename(tokens, rename, replacements.copy(), i)
                i += gg
                eat += gg
        elif tokens[i] == "}":
            i += 1
            bracket_depth -= 1
            if bracket_depth == 0:
                if not found_enum:
                    break
                found_enum = False
        else:
            i += 1

    return eat


def recurse_rename2(tokens: list, rename: set, replacements: dict, i: int = 0):
    eat = 0
    found_enum = False
    bracket_depth = 0
    found_freq = dict()
    start_idx = i
    gen = name_generator()

    while i < len(tokens):
        eat += 1
        if tokens[i] in ["enum", "struct", "class"]:
            found_enum = True
        # Found a word that needs renaming
        elif tokens[i] in rename and tokens[i] not in replacements:
            # Count the word's frequency
            if tokens[i] in found_freq:
                found_freq[tokens[i]] += 1
            else:
                found_freq[tokens[i]] = 1
        elif tokens[i] in ["{", "}"] or i == len(tokens) - 1:
            # Create new replacements
            # Frequency first, then alphabetical
            for name, count in reversed(
                sorted(found_freq.items(), key=lambda item: (item[1], item[0]))
            ):
                if name in replacements:
                    continue

                # Find new replacement
                w = next(gen)
                while w in replacements.values():
                    w = next(gen)

                assert name not in replacements
                assert w not in replacements.values()

                # Add new replacement
                replacements[name] = w

            # Replace what we've found so far
            for idx in range(start_idx, i):
                if tokens[idx] in replacements:
                    tokens[idx] = replacements[tokens[idx]]

            start_idx = i

            # Reset frequency counting
            found_freq = dict()

        if tokens[i] == "{":
            bracket_depth += 1
            if not found_enum:
                gg = recurse_rename2(tokens, rename, replacements.copy(), i + 1)
                i += gg
                eat += gg
                start_idx += gg
                bracket_depth -= 1
        elif tokens[i] == "}":
            bracket_depth -= 1
            if not found_enum:
                break
            if found_enum and bracket_depth <= 0:
                found_enum = False

        i += 1

    assert not found_enum
    assert bracket_depth <= 0

    return eat


def rename_tokens(tokens: list, global_rename: set, rename: set, replacements: dict):
    # Move global generator far enough to avoid conflicts
    # We're going to the end to get names because they should be rare
    gen = name_generator()
    for i in range(len(rename)):
        next(gen)

    # Create global replacements
    # It's possible we might conflict with manual entries, so we still check
    for name in sorted(global_rename):
        w = next(gen)
        while w in replacements:
            w = next(gen)
        replacements[name] = w

    # Rename
    recurse_rename2(tokens, global_rename | rename, replacements.copy())


def get_args():
    parser = argparse.ArgumentParser(description="C++ minifier")
    parser.add_argument("path", help="Path to source file")
    parser.add_argument("--tokens", action="store_true", help="Print tokens")
    parser.add_argument("--output", action="store_true", help="Print minified")
    parser.add_argument("--types", action="store_true", help="Print types")
    parser.add_argument("--names", action="store_true", help="Print names")
    return parser.parse_args()


def get_source(path: str) -> str:
    return open(path, "r").read()


def get_tokens(src: str) -> list:
    return [n for n in re.split("([^a-zA-Z0-9_])", src) if n]


assert get_tokens("test case") == ["test", " ", "case"]
assert get_tokens("test  case") == ["test", " ", " ", "case"]
assert get_tokens("test\t\tcase") == ["test", "\t", "\t", "case"]
assert get_tokens("test_case") == ["test_case"]
assert get_tokens("int x;") == ["int", " ", "x", ";"]
assert get_tokens("x = 1+2;") == ["x", " ", "=", " ", "1", "+", "2", ";"]


def is_name(text: str) -> bool:
    return text and (
        (text[0] == "_" or text[0].isalpha()) and re.match(r"^[A-Za-z0-9_]+$", text)
    )


assert is_name("test")
assert is_name("CAPS")
assert is_name("numbers1")
assert is_name("num2bers")
assert is_name("test_underscore")
assert is_name("_test")
assert is_name("test_")
assert is_name("_test_")
assert is_name("_")
assert is_name("_all__123_STUFF__")
assert is_name("_123")
assert not is_name("123")
assert not is_name("1test")
assert not is_name("1'234")
assert not is_name("0xFF")
assert not is_name("0xFFULL")
assert not is_name("0b101010")
assert not is_name("(")
assert not is_name("[[nodiscard]]")
assert not is_name("")
assert not is_name(" ")
assert not is_name("\t")
assert not is_name("\n")
assert not is_name("\0")
assert not is_name(None)


def is_attachable(part: str) -> bool:
    return part and not (is_name(part) or part.isnumeric())


assert is_attachable("+")
assert is_attachable("{")
assert is_attachable("(")
assert is_attachable("[[nodiscard]]")
assert not is_attachable("is_attachable")
assert not is_attachable("123")
# assert not is_attachable(" ")


def can_attach(part1: str, part2: str) -> bool:
    return is_attachable(part1) or is_attachable(part2)


assert can_attach("+", "=")
assert can_attach("+", "3")
assert can_attach("3", "+")
assert can_attach("int", ",")
assert can_attach("int", ";")
assert can_attach("int", "(")
assert can_attach("[[nodiscard]]", "int")
assert not can_attach("int", "x")
assert not can_attach("auto", "x")
assert not can_attach("return", "x")
assert not can_attach("return", "3")
assert not can_attach("1", "2")
assert not can_attach("const", "int")
# assert not can_attach("x", " ")


def collect_chunks(tokens: list, start: list, end: list, include_end: bool = True):
    new = []

    i = 0
    while i < len(tokens):
        j = i + 1

        if (i + len(start) <= len(tokens)) and (start == tokens[i : i + len(start)]):
            for n in range(j + len(start) - 1, len(tokens) - len(end) + 1):
                if end == tokens[n : n + len(end)]:
                    j = n + len(end) if include_end else n
                    break

        new.append("".join(tokens[i:j]))

        assert j > i
        i = j

    return new


assert collect_chunks(["a", "b", "c", "d"], ["a"], ["b"]) == ["ab", "c", "d"]
assert collect_chunks(["a", "b", "c", "d"], ["b"], ["c"]) == ["a", "bc", "d"]
assert collect_chunks(["a", "b", "c", "d"], ["c"], ["d"]) == ["a", "b", "cd"]
assert collect_chunks(["a", "b", "c", "d"], ["a"], ["d"]) == ["abcd"]
assert collect_chunks(["a", "b", "c", "d"], ["c"], ["b"]) == ["a", "b", "c", "d"]
assert collect_chunks(["a", "b", "c", "d"], ["b"], ["e"]) == ["a", "b", "c", "d"]
assert collect_chunks(["a", "b", "c", "d"], ["b"], ["b"]) == ["a", "b", "c", "d"]
assert collect_chunks(["a", "b", "c", "d"], ["a", "b"], ["c"]) == ["abc", "d"]
assert collect_chunks(["a", "b", "c", "d"], ["a"], ["b", "c"]) == ["abc", "d"]
assert collect_chunks(["a", "b", "c", "d"], ["a", "b", "c"], ["d"]) == ["abcd"]
assert collect_chunks(["a", "b", "c", "d"], ["a"], ["c"], True) == ["abc", "d"]
assert collect_chunks(["a", "b", "c", "d"], ["a"], ["c"], False) == ["ab", "c", "d"]
assert collect_chunks(["a", "b", "c", "d"], ["c"], ["e"]) == ["a", "b", "c", "d"]
assert collect_chunks(["a", "b", "c", "d"], ["e"], ["c"]) == ["a", "b", "c", "d"]


def dissect(src: str, settings: Settings = Settings()) -> str:
    src += "\n"
    tokens: list = get_tokens(src)

    # Collections
    tokens = collect_chunks(tokens, ["/", "/"], ["\n"], False)  # Line comments
    tokens = collect_chunks(tokens, ["/", "*"], ["*", "/"])  # Block comments
    tokens = collect_chunks(tokens, ["[", "["], ["]", "]"])  # Tags
    tokens = collect_chunks(tokens, ['"'], ['"'])  # Strings
    tokens = collect_chunks(tokens, ["'"], ["'"])  # Characters

    # State
    skip_to_end = False
    found_include = False
    found_define = False
    found_template = False
    name_next = False
    global_name_next = False
    type_next = False
    found_enum = False
    num_whitespace = 0
    bracket_depth = 0
    template_depth = 0

    new: list = []
    global_names: set = set()
    names: set = set()
    types: set = set(
        {
            "int",
            "auto",
            "void",
            "bool",
            "char",
            "size_t",
            "int64_t",
            "uint16_t",
            "timespec",
            "mt19937_64",
            "string",
            "array",
            "vector",
            "thread",
            "stringstream",
        }
    )
    our_types: set = set()
    replacements = dict()
    enum_depth = 0

    prev = None
    for token in tokens[:-1]:
        is_whitespace = token in [" ", "\t"]
        is_newline = not prev or prev == "\n"
        is_end = (
            token in [";"]
            or (token == "\n" and found_include)
            or (token == "\n" and found_define)
            or (token == ">" and found_template)
        )
        is_line_comment = token.startswith("//")
        is_block_comment = (
            token.startswith("/*") and token.endswith("*/") and len(token) >= 4
        )

        if skip_to_end and is_end:
            skip_to_end = False
            continue

        # State
        bracket_depth += 1 if token == "(" else -1 if token == ")" else 0
        found_include = found_include or (prev == "#" and token == "include")
        found_define = found_define or (prev == "#" and token == "define")

        found_enum = found_enum or token == "enum"
        if token == "enum":
            enum_depth = 0

        found_template = found_template or prev == "template"
        name_next = name_next or (
            (prev in types or prev in our_types or (found_define and prev == "define"))
            and not found_enum
            and not found_include
        )
        global_name_next = (
            global_name_next or prev == "goto" or (found_define and prev == "define")
        ) and not found_include
        type_next = type_next or prev in [
            "struct",
            "class",
            "typename",
            "using",
            "enum",
        ]

        template_depth += (
            1 if name_next and token == "<" else -1 if name_next and token == ">" else 0
        )

        # Toggle commands
        if is_line_comment:
            parts = token.split(" ")
            if len(parts) == 5 and parts[1] == "minify":
                # // minify enable/disable filter [name]
                if parts[3] == "filter":
                    name = parts[4]
                    if parts[2] == "enable":
                        settings.filters.add(name)
                    elif parts[2] == "disable" and name in settings.filters:
                        settings.filters.remove(name)
                # // minify enable/disable function [name]
                elif parts[3] == "function":
                    name = parts[4]
                    if parts[2] == "disable":
                        settings.deletions.add(name)
                    elif parts[2] == "enable" and name in settings.deletions:
                        settings.deletions.remove(name)
                # // minify replace [from] [to]
                elif parts[2] == "replace":
                    settings.replacements[parts[3]] = parts[4]
                continue

        # Enact filters
        if skip_to_end:
            assert not is_end
            continue
        if "delete" in settings.filters:
            continue
        if "whitespace" in settings.filters and is_whitespace and not found_define:
            continue
        if "const" in settings.filters and token == "const":
            continue
        if "noexcept" in settings.filters and token == "noexcept":
            continue
        if "block_comment" in settings.filters and is_block_comment and len(token) >= 4:
            continue
        if "line_comment" in settings.filters and token.startswith("//"):
            continue
        if "semicolon" in settings.filters and prev == ";" and token == ";":
            continue
        if "nodiscard" in settings.filters and token == "[[nodiscard]]":
            continue
        if "maybe_unused" in settings.filters and token == "[[maybe_unused]]":
            continue
        if token in settings.deletions:
            skip_to_end = True
            continue

        # Sometimes we want to keep newlines - like for #include and #define
        if (
            token == "\n"
            and "whitespace" in settings.filters
            and not found_define
            and not found_include
        ):
            continue

        # #define -- No whitespace inside brackets
        if (
            "whitespace" in settings.filters
            and is_whitespace
            and found_define
            and bracket_depth != 0
        ):
            continue

        # #define -- Don't double tap whitespace
        if (
            "whitespace" in settings.filters
            and is_whitespace
            and found_define
            and prev in [" ", "\t"]
        ):
            continue

        # Count whitespace
        num_whitespace += 1 if token in [" ", "\t"] else 0

        # Add a separator if we can't attach the new token to the last
        if (
            prev
            and not can_attach(prev, token)
            and (not found_enum or "enum" not in settings.filters)
        ):
            new.append(" ")

        # False alarm on an upcoming type
        if type_next and token in ["namespace", "{"]:
            type_next = False

        # False alarm on an upcoming name
        if name_next and token in [",", "{", "=", ")"] and template_depth == 0:
            name_next = False

        # Update the set of types
        if (
            type_next
            and is_name(token)
            and token not in types
            and token not in our_types
            and not (found_enum and token in ["struct", "class"])
        ):
            our_types.add(token)
            type_next = False

        # Update the set of names
        if name_next and is_name(token) and template_depth == 0:
            # Some things we shouldn't rename
            if token not in ["main", "operator"]:
                names.add(token)
            name_next = False
        elif global_name_next and is_name(token):
            global_names.add(token)
            global_name_next = False
        elif (
            found_enum
            and is_name(token)
            and token not in ["enum", "struct", "class"]
            and token not in types
            and token not in our_types
        ):
            if "enum" in settings.filters:
                replacements[token] = str(enum_depth)
                enum_depth += 1
            else:
                names.add(token)

        if token == "\n" and "trailing_whitespace" in settings.filters:
            while new[-1:] in [" ", "\t"]:
                new.pop()

        if found_enum and "enum" in settings.filters:
            pass
        else:
            new.append(token)

        # Reset state
        if is_end:
            # skip_to_end = False
            found_include = False
            found_define = False
            found_template = False

            name_next = False
            global_name_next = False
            type_next = False

            found_enum = False
            num_whitespace = 0
            bracket_depth = 0
            template_depth = 0

        prev = token

    if "trailing_whitespace" in settings.filters:
        while len(new) > 0 and new[-1:][0] in [" ", "\t"]:
            new.pop()

    if "trailing_newline" in settings.filters:
        while len(new) > 0 and new[-1:][0] == "\n":
            new.pop()

    return new, our_types, global_names, names, replacements


# Assert types
assert dissect("int x;")[1] == set()
assert dissect("struct x;")[1] == set(["x"])
assert dissect("using x = int;")[1] == set(["x"])
assert dissect("template <typename T>auto func(){}")[1] == set(["T"])
assert dissect("template <typename S, typename T>auto func(){}")[1] == set(["S", "T"])
assert dissect("enum {a, b, c};")[1] == set()
assert dissect("enum Test {a, b, c};")[1] == set(["Test"])
assert dissect("enum class Test {a, b, c};")[1] == set(["Test"])
assert dissect("enum class Test : int {a, b, c};")[1] == set(["Test"])
# Assert global names
assert dissect("int x;")[2] == set()
assert dissect("struct x;")[2] == set()
assert dissect("using x = int;")[2] == set()
assert dissect("template <typename T>auto func(){}")[2] == set()
assert dissect("template <typename S, typename T>auto func(){}")[2] == set()
assert dissect("enum {a, b, c};")[2] == set()
assert dissect("enum Test {a, b, c};")[2] == set()
assert dissect("enum class Test {a, b, c};")[2] == set()
assert dissect("enum class Test : int {a, b, c};")[2] == set()
# Assert names
assert dissect("int x;")[3] == set(["x"])
assert dissect("struct x;")[3] == set()
assert dissect("using x = int;")[3] == set()
assert dissect("template <typename T>auto func(){}")[3] == set(["func"])
assert dissect("template <typename S, typename T>auto func(){}")[3] == set(["func"])
assert dissect("enum {a, b, c};")[3] == set(["a", "b", "c"])
assert dissect("enum Test {a, b, c};")[3] == set(["a", "b", "c"])
assert dissect("enum class Test {a, b, c};")[3] == set(["a", "b", "c"])
assert dissect("enum class Test : int {a, b, c};")[3] == set(["a", "b", "c"])
# Assert enum replacements
assert dissect("enum {a, b, c};")[4] == dict()
assert dissect("enum Test {a, b, c};")[4] == dict()
assert dissect("enum class Test {a, b, c};")[4] == dict()
assert dissect("enum class Test : int {a, b, c};")[4] == dict()
assert dissect("// minify enable filter enum\nenum {a, b, c};")[4] == dict(
    {"a": "0", "b": "1", "c": "2"}
)
assert dissect("// minify enable filter enum\nenum Test {a, b, c};")[4] == dict(
    {"a": "0", "b": "1", "c": "2"}
)
assert dissect("// minify enable filter enum\nenum class Test {a, b, c};")[4] == dict(
    {"a": "0", "b": "1", "c": "2"}
)
assert dissect("// minify enable filter enum\nenum class Test : int {a, b, c};")[
    4
] == dict({"a": "0", "b": "1", "c": "2"})
# This is not implemented
# assert dissect("// minify enable filter enum\nenum {a = 3, b, c};")[4] == dict(
#     {"a": "3", "b": "4", "c": "5"}
# )


def minify(src: str, settings: Settings = Settings()) -> str:
    tokens, our_types, global_names, names, replacements = dissect(src, settings)

    for a, b in replacements.items():
        settings.replacements[a] = b

    # Time to rename stuff
    if settings.rename:
        rename_tokens(
            tokens,
            global_names,
            names | our_types,
            settings.replacements,
        )

    return "".join(tokens)


assert minify("") == ""
assert minify(" ") == ""
assert minify("\t") == ""
assert minify("\n") == ""
assert minify("int x;") == "int x;"
assert minify("int x = 1;") == "int x=1;"
assert minify("return age + 1 * 3;") == "return age+1*3;"
assert minify("auto func(const char c, int);") == "auto func(char c,int);"
assert minify("int /* test */ var;") == "int var;"
assert minify("int var; // test") == "int var;"
assert minify("int *var;") == "int*var;"
assert minify("int* var;") == "int*var;"
assert minify("int **var;") == "int**var;"
assert minify("int** var;") == "int**var;"
assert minify("int** * **var;") == "int*****var;"
assert minify("int &var;") == "int&var;"
assert minify("int& var;") == "int&var;"
assert minify("func();;") == "func();"
assert minify("[[nodiscard]] int func();") == "int func();"
assert minify("[[maybe_unused]] int func();") == "int func();"
assert minify("#include <iostream>") == "#include<iostream>"
assert minify("x;    ") == "x;"
assert minify("x;\n") == "x;"
assert minify("x;   \n") == "x;"
assert minify("\n\nx;\n\n") == "x;"
assert minify("x;\nx;") == "x;x;"
assert minify("x;\nx;\n") == "x;x;"
assert minify("#define true false") == "#define true false"
assert minify("#define   true false") == "#define true false"
assert minify("#define true   false") == "#define true false"
assert minify("#define true false   ") == "#define true false"
assert minify("#define add_2(x) (x + 2)") == "#define add_2(x) (x+2)"
assert minify("#define add(x, y) (x + y)") == "#define add(x,y) (x+y)"
assert minify("#define add(x, y, z) (x + y + z)") == "#define add(x,y,z) (x+y+z)"
assert minify("#define add( x , y , z ) ( x + y + z )") == "#define add(x,y,z) (x+y+z)"
# This isn't legal C++
# assert minify("#define add (x, y, z) (x + y + z)") == "#define add(x,y,z) (x+y+z)"
assert (
    minify("template <typename S, typename T>auto func(){}")
    == "template<typename S,typename T>auto func(){}"
)
assert minify("struct [[nodiscard]] test(){};") == "struct test(){};"
assert minify("class [[nodiscard]] test(){};") == "class test(){};"
assert minify("enum {a, b, c};") == "enum{a,b,c};"
assert minify("enum { a = 3, b = 4, c = 5 };") == "enum{a=3,b=4,c=5};"
assert minify("enum class {a, b, c};") == "enum class{a,b,c};"
assert minify("enum class : int {a, b, c};") == "enum class:int{a,b,c};"
assert minify("enum : int {a, b, c};") == "enum:int{a,b,c};"
assert minify("// minify enable filter enum\nenum {a, b, c};") == ""
assert minify("// minify enable filter enum\nenum : int {a, b, c};") == ""
assert minify("// minify enable filter enum\nenum class Test : int {a, b, c};") == ""
assert (
    minify("// minify enable filter enum\nint n;\nenum {a, b, c};\nint m;")
    == "int n;int m;"
)
assert (
    minify("// minify enable filter enum\nint test;\nenum class Test : int {a, b, c};")
    == "int test;"
)
assert (
    minify("// minify enable filter enum\nenum class Test : int {a, b, c};\nint test;")
    == "int test;"
)
assert minify("// minify enable filter enum\nenum {a};\nint test;") == "int test;"
assert (
    minify("// minify enable filter enum\nenum class Test : int {a};\nint test;")
    == "int test;"
)
assert (
    minify("// minify enable filter enum\nint test1;\nenum {a, b, c};\nint test2;")
    == "int test1;int test2;"
)
assert (
    minify(
        "// minify enable filter enum\nint test1;\nenum Test int {a, b, c};\nint test2;"
    )
    == "int test1;int test2;"
)
assert (
    minify(
        "// minify enable filter enum\nint test1;\nenum class Test {a, b, c};\nint test2;"
    )
    == "int test1;int test2;"
)
assert (
    minify("// minify enable filter enum\nint test1;\nenum Test {a, b, c};\nint test2;")
    == "int test1;int test2;"
)
assert (
    minify(
        "// minify enable filter enum\nint test1;\nenum class Test : int {a, b, c};\nint test2;"
    )
    == "int test1;int test2;"
)


def main():
    args = get_args()

    if not args.path:
        print("Path is required")
        return

    if not os.path.exists(args.path):
        print("File doesn't exist")
        return

    src: str = get_source(args.path)
    settings: Settings = Settings()
    settings.rename = True

    if args.output:
        print(minify(src, settings))
    elif args.tokens:
        tokens, our_types, global_names, names, _ = dissect(src, settings)
        print(tokens)
    elif args.types:
        tokens, our_types, global_names, names, _ = dissect(src, settings)
        print(our_types)
    elif args.names:
        tokens, our_types, global_names, names, _ = dissect(src, settings)
        print(global_names | names)
    else:
        print(minify(src, settings))


if __name__ == "__main__":
    main()
