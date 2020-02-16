namespace Mal;

interface Form {}

class ListLikeForm implements Form {
  public function __construct(public vec<Form> $children) {}
}

final class ListForm extends ListLikeForm {}

final class VectorForm extends ListLikeForm {}

final class HashMapForm implements Form {
  public function __construct(public dict<string, Form> $map) {}
}

interface Atom extends Form {}

final class Number implements Atom {
  public function __construct(public int $value) {}
}

interface Key extends Atom {}

final class Keyword implements Key {
  public function __construct(public string $name) {}
}

final class StringAtom implements Key {
  public function __construct(public string $value) {}
}

final class BoolAtom implements Atom {
  public function __construct(public bool $value) {}
}

final class Symbol implements Atom {
  public function __construct(public string $name) {}
}

final class GlobalNil implements Atom {}

interface FunctionLike extends Atom {}

final class FunctionDefinition implements FunctionLike {
  public function __construct(public (function(vec<Form>): Form) $function) {}
}

final class FunctionWithTCODefinition implements FunctionLike {
  public function __construct(
    public Form $body,
    public vec<Symbol> $parameters,
    public Environment $closed_over_environment,
    public FunctionDefinition $unoptimized,
  ) {}
}

final class MutableAtom implements Atom {
  public function __construct(public Form $value) {}

  public function reset(Form $new_value): Form {
    $this->value = $new_value;
    return $new_value;
  }
}
