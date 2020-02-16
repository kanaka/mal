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
        "Expected a form of type %s, but got `%s`",
        Regex\replace($type, re"/\w+\\\\/", ''),
        pr_str($got, true),
      ),
    );
  }
}

final class EvalTypedArgumentException extends \Error {
  public function __construct(
    string $function_name,
    int $index,
    classname<Form> $type,
    ?Form $got,
    ?string $argument_description = null,
  ) {
    parent::__construct(
      Str\format(
        "`%s` expected %s%s argument of type %s, but got `%s`",
        $function_name,
        $argument_description is null ? '' : $argument_description.' as ',
        index_to_adjective($index),
        Regex\replace($type, re"/\w+\\\\/", ''),
        $got is null ? 'none' : '`'.pr_str($got, true).'`',
      ),
    );
  }
}

final class EvalUntypedArgumentException extends \Error {
  public function __construct(
    string $function_name,
    int $index,
    ?string $argument_description = null,
  ) {
    parent::__construct(
      Str\format(
        "`%s` expected %s%s argument, but got none",
        $function_name,
        $argument_description is null ? '' : $argument_description.' as ',
        index_to_adjective($index),
      ),
    );
  }
}

final class EvalArityException extends \Error {
  public function __construct(
    string $function_name,
    int $max_num_arguments,
    vec<Form> $arguments,
  ) {
    parent::__construct(
      Str\format(
        "`%s` expected only %d argument%s, but got %d: `%s`",
        $function_name,
        $max_num_arguments,
        ($max_num_arguments === 1 ? '' : 's'),
        C\count($arguments) - 1,
        pr_str_arguments($arguments, true, ' '),
      ),
    );
  }
}

function index_to_adjective(int $index): string {
  switch ($index) {
    case 1:
      return '1st';
    case 2:
      return '2nd';
    case 3:
      return '3rd';
    default:
      return $index."th";
  }
}
