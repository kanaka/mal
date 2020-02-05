namespace Mal;

type Token = string;

function read_str(string $mal_code): Form {
  $tokens = tokenize($mal_code);
  list($ast, $_reader) = read_form(new Reader($tokens));
  return $ast;
}

final class Reader {
  public function __construct(
    private vec<Token> $tokens,
    private int $index = 0,
  ) {}

  public function peek(): ?Token {
    return idx($this->tokens, $this->index);
  }

  public function peekx(string $error_message): Token {
    $token = $this->peek();
    if ($token is null) {
      throw new ReaderException($error_message, $this->index);
    }
    return $token;
  }

  public function next(): Reader {
    return new Reader($this->tokens, $this->index + 1);
  }
}

function tokenize(string $mal_code): vec<Token> {
  $matches = \HH\Lib\Regex\every_match(
    $mal_code,
    // Matches all mal tokens
    re"/[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|;.*|[^\s\[\]{}('\"`,;)]*)/",
  );
  return Vec\map($matches, $match ==> $match[1]);
}

function read_form(Reader $token_reader): (Form, Reader) {
  $first_token = $token_reader->peekx('Expected a form');
  return $first_token === '('
    ? read_list($token_reader)
    : read_atom($token_reader);
}

function read_list(Reader $token_reader): (ListForm, Reader) {
  $children = vec[];
  while (true) {
    $token_reader = $token_reader->next();
    if ($token_reader->peekx('Expected a form or `)`') === ')') {
      return tuple(new ListForm($children), $token_reader);
    } else {
      list($child, $token_reader) = read_form($token_reader);
      $children[] = $child;
    }
  }
}

function read_atom(Reader $token_reader): (Atom, Reader) {
  $token = $token_reader->peekx('Expected an atom');
  if (Regex\matches($token, re"/^-?\d/")) {
    return tuple(new Number((int)$token), $token_reader);
  } else {
    return tuple(new Symbol($token), $token_reader);
  }
}
