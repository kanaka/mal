namespace Mal;

interface Form {}

final class ListForm implements Form {
  public function __construct(public vec<Form> $children) {}
}

final class VectorForm implements Form {
  public function __construct(public vec<Form> $children) {}
}

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

final class FunctionDefinition implements Atom {
  public function __construct(public (function(vec<Form>): Form) $function) {}
}
