import mal_readline


def read(source: str) -> str:
    return source


def eval_(ast: str) -> str:
    return ast


def print_(form: str) -> str:
    return form


def rep(source: str) -> str:
    return print_(eval_(read(source)))


def main() -> None:
    while True:
        try:
            print(rep(mal_readline.input_('user> ')))
        except EOFError:
            break


if __name__ == '__main__':
    main()
