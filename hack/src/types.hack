namespace Mal;

interface Form {}

final class ListForm implements Form {
  public function __construct(public vec<Form> $children) {}
}

interface Atom extends Form {}

final class Number implements Atom {
  public function __construct(public int $value) {}
}

final class Symbol implements Atom {
  public function __construct(public string $name) {}
}
