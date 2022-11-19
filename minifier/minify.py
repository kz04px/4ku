import argparse
import os.path

def get_args():
    parser = argparse.ArgumentParser(description='C++ minifier')
    parser.add_argument('src', help='Path to source file')
    return parser.parse_args()

def get_source(path):
    return open(path, "r").read()

def main():
    args = get_args()

    if not os.path.isfile(args.src):
        raise exception("File not found")

    src = get_source(args.src)

    print(src, end="")

if __name__ == "__main__":
    main()
