namespace Mal;

final class ReaderException extends \Error {
  public function __construct(string $message, public int $index) {
    parent::__construct($message);
  }
}

final class EvalException extends \Error {
  public function __construct(string $message) {
    parent::__construct($message);
  }
}

final class EvalTypeException extends \Error {
  public function __construct(classname<Form> $type, Form $got) {
    parent::__construct(
      Str\format(
        "Runtime type exception: Expected a %s, but got `%s`",
        Regex\replace($type, re"/\w+\\\\/", ''),
        pr_str($got, true),
      ),
    );
  }
}
