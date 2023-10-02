import sys
from pathlib import Path

def run(source):
    scanner = Scanner(source)
    tokens = scanner.scan_tokens()
    parser = Parser(tokens)
    statements = parser.parse()

def run_file(path_string):
    try:
        with Path(path_string).open() as file:
            run(file) # Run the whole file in totality.
    except e:
        print(e)

# This is the main entry point for the python prototype
def main():
    if len(sys.argv) > 1:
        print("Usage: main.py [script file]")
    elif len(sys.arv) is 1:
        run_file(sys.argv[1])
    else:
        # A REPL can be called here later.
        print("Usage: main.py [script file]")

if __name__ == "__main__":
    main()