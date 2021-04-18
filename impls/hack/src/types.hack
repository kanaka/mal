namespace Mal;

interface Form {}

interface WithMetadata {
  public function metadata(): ?Form;
}

trait WithMetadataTrait implements WithMetadata {
  private ?Form $metadata;

  public function metadata(): ?Form {
    return $this->metadata;
  }
}

abstract class ListLikeForm implements Form {
  use WithMetadataTrait;

  public function __construct(
    public vec<Form> $children,
    private ?Form $metadata = null,
  ) {}
}

final class ListForm extends ListLikeForm {}

final class VectorForm extends ListLikeForm {}

final class HashMapForm implements Form {
  use WithMetadataTrait;

  public function __construct(
    public dict<string, Form> $map,
    private ?Form $metadata = null,
  ) {}
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
  use WithMetadataTrait;

  public function __construct(
    public (function(vec<Form>): Form) $function,
    private ?Form $metadata = null,
  ) {}
}

final class FunctionWithTCODefinition implements FunctionLike {
  use WithMetadataTrait;

  public function __construct(
    public bool $is_macro,
    public Form $body,
    public vec<Symbol> $parameters,
    public Environment $closed_over_environment,
    public FunctionDefinition $unoptimized,
    private ?Form $metadata = null,
  ) {}
}

final class MutableAtom implements Atom {
  public function __construct(public Form $value) {}

  public function reset(Form $new_value): Form {
    $this->value = $new_value;
    return $new_value;
  }
}
