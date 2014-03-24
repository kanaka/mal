<?php

require_once 'types.php';

class Reader {
    protected $tokens = array();
    protected $position = 0;
    public function __construct($tokens) {
        $this->tokens = $tokens;
        $this->position = 0;
    }
    public function next() {
        return $this->tokens[$this->position++];
    }
    public function peek() {
        return $this->tokens[$this->position];
    }
}

class BlankException extends Exception {
}

function _real_token($s) {
    return $s !== '' && $s[0] !== ';';
}

function tokenize($str) {
    $pat = "/[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;.*|[^\s\[\]{}('\"`,;)]*)/";
    preg_match_all($pat, $str, $matches);
    return array_values(array_filter($matches[1], '_real_token'));
}

function read_atom($reader) {
    $token = $reader->next();
    if (preg_match("/^-?[0-9]+$/", $token)) {
        return intval($token, 10);
    } elseif ($token[0] === "\"") {
        $str = substr($token, 1, -1);
        $str = preg_replace('/\\\\"/', '"', $str);
        return $str;
    } elseif ($token === "nil") {
        return NULL;
    } elseif ($token === "true") {
        return true;
    } elseif ($token === "false") {
        return false;
    } else {
        return new_symbol($token);
    }
}

function read_list($reader, $constr='new_list', $start='(', $end=')') {
    $ast = $constr();
    $token = $reader->next();
    if ($token !== $start) {
        throw new Exception("expected '" . $start . "'");
    }
    while (($token = $reader->peek()) !== $end) {
        if ($token === "") {
            throw new Exception("expected '" . $end . "', got EOF");
        }
        $ast[] = read_form($reader);
    }
    $reader->next();
    return $ast;
}

function read_hash_map($reader) {
    $lst = read_list($reader, 'new_list', '{', '}');
    return call_user_func_array('new_hash_map', $lst->getArrayCopy());
}

function read_form($reader) {
    $token = $reader->peek();
    switch ($token) {
    case '\'': $reader->next();
               return new_list(new_symbol('quote'),
                               read_form($reader));
    case '`':  $reader->next();
               return new_list(new_symbol('quasiquote'),
                               read_form($reader));
    case '~':  $reader->next();
               return new_list(new_symbol('unquote'),
                               read_form($reader));
    case '~@': $reader->next();
               return new_list(new_symbol('splice-unquote'),
                               read_form($reader));
    case '^':  $reader->next();
               $meta = read_form($reader);
               return new_list(new_symbol('with-meta'),
                               read_form($reader),
                               $meta);

    case '@':  $reader->next();
               return new_list(new_symbol('deref'),
                               read_form($reader));

    case ')': throw new Exception("unexpected ')'");
    case '(': return read_list($reader);
    case ']': throw new Exception("unexpected ']'");
    case '[': return read_list($reader, 'new_vector', '[', ']');
    case '}': throw new Exception("unexpected '}'");
    case '{': return read_hash_map($reader);

    default:  return read_atom($reader);
    }
}

function read_str($str) {
    $tokens = tokenize($str);
    if (count($tokens) === 0) { throw new BlankException(); }
    return read_form(new Reader($tokens));
}

?>
