import traceback

import mal_readline

from mal_types import Form

import reader


def eval_(ast: Form) -> Form:
    # print(repr(ast))  # the result of read, as python
    return ast


def rep(source: str) -> str:
    return str(eval_(reader.read(source)))


def main() -> None:
    while True:
        try:
            print(rep(mal_readline.input_('user> ')))
        except EOFError:
            break
        # pylint: disable-next=broad-exception-caught
        except Exception as exc:
            traceback.print_exception(exc, limit=10)


if __name__ == '__main__':
    main()
