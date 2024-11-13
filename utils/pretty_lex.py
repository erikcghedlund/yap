class Token:
    def __init__(self, s):
        self.s = s
        self.line = int(s.split(",")[-1])

    def __str__(self):
        return self.s


def main():
    tokens = [Token(s) for s in input()[1:-1].split("}{")]
    for i in range(1, tokens[-1].line):
        print(", ".join([str(tok) for tok in tokens if tok.line == i]))


if __name__ == "__main__":
    main()
