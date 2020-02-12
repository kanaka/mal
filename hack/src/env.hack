namespace Mal;

final class Environment {
  public function __construct(
    private ?Environment $outer = null,
    private dict<string, Form> $data = dict[],
  ) {
  }

  public function set(Symbol $symbol, Form $value): Form {
    $this->data[$symbol->name] = $value;
    return $value;
  }

  public function find(Symbol $symbol): ?this {
    if (C\contains_key($this->data, $symbol->name)) {
      return $this;
    }
    return $this->outer?->find($symbol);
  }

  public function get(Symbol $symbol): Form {
    $environment = $this->find($symbol);
    if ($environment is null) {
      throw new EvalException(
        'Definition for symbol \''.$symbol->name.'\' not found in scope',
      );
    }
    return $environment->data[$symbol->name];
  }
}
