namespace Mal;

final class ReaderException extends \Error {
  public function __construct(string $message, public int $index) {
    parent::__construct($message);
  }
}
