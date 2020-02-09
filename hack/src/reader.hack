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
      throw $this->exception($error_message);
    }
    return $token as nonnull;
  }

  public function next(): Reader {
    return new Reader($this->tokens, $this->index + 1);
  }

  public function exception(string $error_message): ReaderException {
    return new ReaderException($error_message, $this->index);
  }
}

function tokenize(string $mal_code): vec<Token> {
  $matches = \HH\Lib\Regex\every_match(
    $mal_code,
    // Matches all mal tokens
    re"/[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\.|[^\\\"])*\"?|;.*|[^\s\[\]{}('\"`,;)]*)/",
  );
  return $matches
    |> Vec\map($$, $match ==> $match[1])
    |> Vec\filter($$, $token ==> !Str\is_empty($token));
}

function read_form(Reader $token_reader): (Form, Reader) {
  return read_list($token_reader) ??
    read_vector($token_reader) ??
    read_hash_map($token_reader) ??
    read_atom($token_reader);
}

function read_list(Reader $token_reader): ?(ListForm, Reader) {
  return read_children_form(
    $token_reader,
    '(',
    ')',
    ($children, $_) ==> new ListForm($children),
  );
}

function read_vector(Reader $token_reader): ?(VectorForm, Reader) {
  return read_children_form(
    $token_reader,
    '[',
    ']',
    ($children, $_) ==> new VectorForm($children),
  );
}

function read_hash_map(Reader $token_reader): ?(HashMapForm, Reader) {
  return read_children_form(
    $token_reader,
    '{',
    '}',
    ($children, $token_reader) ==>
      pairs_to_map(read_pairs($children, $token_reader)),
  );
}

function read_children_form<TForm>(
  Reader $token_reader,
  Token $start_token,
  Token $end_token,
  (function(vec<Form>, Reader): TForm) $create_node,
): ?(TForm, Reader) {
  $first_token = $token_reader->peekx('Expected a form');
  if ($first_token !== $start_token) {
    return null;
  }
  $children = vec[];
  while (true) {
    $token_reader = $token_reader->next();
    if (
      $token_reader->peekx("Expected a form or `$end_token`") === $end_token
    ) {
      return tuple($create_node($children, $token_reader), $token_reader);
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
  } else if (Str\starts_with($token, ':')) {
    return tuple(new Keyword(Str\slice($token, 1)), $token_reader);
  } else {
    return tuple(new Symbol($token), $token_reader);
  }
}


function read_pairs(vec<Form> $list, Reader $token_reader): vec<(Key, Form)> {
  $num_items = C\count($list);
  $pairs = vec[];
  for ($i = 0; $i < $num_items; $i += 2) {
    $key = $list[$i];
    if (!$key is Key) {
      throw $token_reader->exception("Expected a key atom");
    }
    if ($i + 1 >= $num_items) {
      throw $token_reader->exception("Expected a value atom");
    }
    $pairs[] = tuple($key, $list[$i + 1]);
  }
  return $pairs;
}

function pairs_to_map(vec<(Key, Form)> $children_pairs): HashMapForm {
  $map = dict[];
  foreach ($children_pairs as $key_value_pair) {
    list($key, $value) = $key_value_pair;
    $map[key_to_string($key)] = $value;
  }
  return new HashMapForm($map);
}

function children_pairs(HashMapForm $form): vec<(Key, Form)> {
  $pairs = vec[];
  foreach ($form->map as $key => $value) {
    $pairs[] = tuple(string_to_key($key), $value);
  }
  return $pairs;
}

function key_to_string(Key $key): string {
  if ($key is Keyword) {
    return "\u{29e}".$key->name;
  }
  invariant(false, 'Unsupported Key subtype');
}

function string_to_key(string $key): Key {
  if (Str\starts_with($key, "\u{29e}")) {
    return new Keyword(Str\slice($key, 2));
  }
  invariant(false, 'Unsupported Key subtype');
}
