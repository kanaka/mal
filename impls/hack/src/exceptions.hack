namespace Mal;

final class ThrownException extends \Error {
  public function __construct(public Form $thrown) {
    parent::__construct(
      Str\format("Throw exception with `%s`", pr_str($thrown, true)),
    );
  }
}

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

final class TypeException extends \Error {
  public function __construct(
    string $function_name,
    classname<Form> $type,
    Form $got,
    ?string $argument_description = null,
  ) {
    parent::__construct(
      call_exception(
        $function_name,
        type_exception($type, $argument_description),
        received_form($got),
      ),
    );
  }
}

final class TypedArgumentException extends \Error {
  public function __construct(
    string $function_name,
    int $index,
    classname<Form> $type,
    ?Form $got,
    ?string $argument_description = null,
  ) {
    parent::__construct(
      call_exception(
        $function_name,
        type_exception(
          $type,
          argument_exception($index, $argument_description),
        ),
        received_form($got),
      ),
    );
  }
}

final class MissingArgumentException extends \Error {
  public function __construct(
    string $function_name,
    int $index,
    ?string $argument_description = null,
  ) {
    parent::__construct(
      call_exception(
        $function_name,
        argument_exception($index, $argument_description),
        received_form(null),
      ),
    );
  }
}

final class ArityException extends \Error {
  public function __construct(
    string $function_name,
    int $max_num_arguments,
    vec<Form> $arguments,
  ) {
    parent::__construct(
      call_exception(
        $function_name,
        arity_exception($max_num_arguments),
        received_arity($arguments),
      ),
    );
  }
}

function call_exception(
  string $function_name,
  string $error_message,
  string $got,
): string {
  return Str\format(
    "`%s` expected %s, but got %s",
    $function_name,
    $error_message,
    $got,
  );
}

function received_form(?Form $got): string {
  return $got is null ? 'none' : '`'.pr_str($got, true).'`';
}

function type_exception(
  classname<Form> $type,
  ?string $argument_description,
): string {
  return Str\format(
    "%s of type %s",
    $argument_description is null ? 'a form' : $argument_description,
    Regex\replace($type, re"/\w+\\\\/", ''),
  );
}

function argument_exception(
  int $index,
  ?string $argument_description = null,
): string {
  return Str\format(
    "%s%s argument",
    $argument_description is null ? '' : $argument_description.' as ',
    index_to_adjective($index),
  );
}

function arity_exception(int $max_num_arguments): string {
  return Str\format(
    "only %d argument%s",
    $max_num_arguments,
    ($max_num_arguments === 1 ? '' : 's'),
  );
}

function received_arity(vec<Form> $arguments): string {
  return Str\format(
    "%d: `%s`",
    C\count($arguments) - 1,
    pr_str_arguments($arguments, true, ' '),
  );
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
