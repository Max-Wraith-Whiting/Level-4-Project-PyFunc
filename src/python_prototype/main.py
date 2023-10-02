import sys

# This is the main entry point for the python prototype
def main():
    if len(sys.argv) > 1:
        print("Usage: main.py [script file]")
    elif len(sys.arv) is 1:
        runFile(sys.argv[1])
    else:
        # A REPL can be called here later.
        print("Usage: main.py [script file]")

if __name__ == "__main__":
    main()