<?php

function _pr_str($obj, $print_readably=True) {
    if (list_Q($obj)) {
        $ret = array();
        foreach ($obj as $e) {
            array_push($ret, _pr_str($e, $print_readably));
        }
        return "(" . implode(" ", $ret) . ")";
    } elseif (vector_Q($obj)) {
        $ret = array();
        foreach ($obj as $e) {
            array_push($ret, _pr_str($e, $print_readably));
        }
        return "[" . implode(" ", $ret) . "]";
    } elseif (hash_map_Q($obj)) {
        $ret = array();
        foreach (array_keys($obj->getArrayCopy()) as $k) {
            $ret[] = _pr_str($k, $print_readably);
            $ret[] = _pr_str($obj[$k], $print_readably);
        }
        return "{" . implode(" ", $ret) . "}";
    } elseif (is_string($obj)) {
        if ($print_readably) {
            $obj = preg_replace('/"/', '\\"', preg_replace('/\\\\/', '\\\\\\\\', $obj));
            return '"' . $obj . '"';
        } else {
            return $obj;
        }
    } elseif (is_integer($obj)) {
        return $obj;
    } elseif ($obj === NULL) {
        return "nil";
    } elseif ($obj === true) {
        return "true";
    } elseif ($obj === false) {
        return "false";
    } elseif (symbol_Q($obj)) {
        return $obj->value;
    } elseif (atom_Q($obj)) {
        return "(atom " . _pr_str($obj->value, $print_readably) . ")";
    } elseif (function_Q($obj)) {
        return "(fn* [...] ...)";
    } elseif (is_callable($obj)) {  // only step4 and below
        return "#<function ...>";
    } else {
        throw new Exception("_pr_str unknown type: " . gettype($obj));
    }
}

function pr_str() {
    $ps = array_map(function ($obj) { return _pr_str($obj, True); },
                    func_get_args());
    return implode(" ", $ps);
}

function str() {
    $ps = array_map(function ($obj) { return _pr_str($obj, False); },
                    func_get_args());
    return implode("", $ps);
}

function prn() {
    $ps = array_map(function ($obj) { return _pr_str($obj, True); },
                    func_get_args());
    print implode(" ", $ps) . "\n";
    return null;
}

function println() {
    $ps = array_map(function ($obj) { return _pr_str($obj, False); },
                    func_get_args());
    print implode(" ", $ps) . "\n";
    return null;
}

function with_meta($obj, $m) {
    $new_obj = clone $obj;
    $new_obj->meta = $m;
    return $new_obj;
}

function meta($obj) {
    return $obj->meta;
}

function equal_Q($a, $b) {
    $ota = gettype($a) === "object" ? get_class($a) : gettype($a);
    $otb = gettype($b) === "object" ? get_class($b) : gettype($b);
    if (!($ota === $otb or (sequential_Q($a) and sequential_Q($b)))) {
        return false;
    } elseif (symbol_Q($a)) {
        #print "ota: $ota, otb: $otb\n";
        return $a->value === $b->value;
    } elseif (list_Q($a) or vector_Q($a)) {
        if ($a->count() !== $b->count()) { return false; }
        for ($i=0; $i<$a->count(); $i++) {
            if (!equal_Q($a[$i], $b[$i])) { return false; }
        }
        return true;
    } else {
        return $a === $b;
    }
}

// nil, true, false, string
function nil_Q($obj) { return $obj === NULL; }
function true_Q($obj) { return $obj === true; }
function false_Q($obj) { return $obj === false; }
function string_Q($obj) { return is_string($obj); }


// symbols
class SymbolClass {
    public $value = NULL;
    public $meta = NULL;
    public function __construct($value) {
        $this->value = $value;
    }
}

function new_symbol($name) { return new SymbolClass($name); }

function symbol_Q($obj) { return ($obj instanceof SymbolClass); }


// Functions
class FunctionClass {
    public $func = NULL;
    public $type = 'native';   // 'native' or 'platform'
    public $meta = NULL;
    public $ismacro = False;
    public function __construct($func, $type, $meta=NULL, $ismacro=False) {
        $this->func = $func;
        $this->type = $type;
        $this->meta = $meta;
        $this->ismacro = $ismacro;
    }
    public function __invoke() {
        $args = func_get_args();
        if ($this->type === 'native') {
            $fn_env = new Env($this->meta['env'],
                              $this->meta['params'], $args);
            $evalf = $this->func;
            return $evalf($this->meta['exp'], $fn_env);
        } else {
            return call_user_func_array($this->func, $args);
        }
    }
    public function apply($args) {
        return call_user_func_array(array(&$this, '__invoke'),$args);
    }
}

function new_function($func, $type='platform', $meta=NULL, $ismacro=False) {
    return new FunctionClass($func, $type, $meta, $ismacro);
}

function function_Q($obj) { return $obj instanceof FunctionClass; }

// Parent class of list, vector, hash-map
// http://www.php.net/manual/en/class.arrayobject.php
class SeqClass extends ArrayObject {
    public function slice($start, $length=NULL) {
        $sc = new $this();
        if ($start >= count($this)) {
            $arr = array();
        } else {
            $arr = array_slice($this->getArrayCopy(), $start, $length);
        }
        $sc->exchangeArray($arr);
        return $sc;
    }
}


// Hash Maps
class HashMapClass extends ArrayObject {
    public $meta = NULL;
}

function new_hash_map() {
    $args = func_get_args();
    if (count($args) % 2 === 1) {
        throw new Exception("Odd number of hash map arguments");
    }
    $hm = new HashMapClass();
    array_unshift($args, $hm);
    return call_user_func_array('assoc_BANG', $args);
}

function hash_map_Q($obj) { return $obj instanceof HashMapClass; }

function assoc_BANG($hm) {
    $args = func_get_args();
    if (count($args) % 2 !== 1) {
        throw new Exception("Odd number of assoc arguments");
    }
    for ($i=1; $i<count($args); $i+=2) {
        $ktoken = $args[$i];
        $vtoken = $args[$i+1];
        // TODO: support more than string keys
        if (gettype($ktoken) !== "string") {
            throw new Exception("expected hash-map key string, got: " . gettype($ktoken));
        }
        $hm[$ktoken] = $vtoken;
    }
    return $hm;
}

function assoc($src_hm) {
    $args = func_get_args();
    $hm = clone $src_hm;
    $args[0] = $hm;
    return call_user_func_array('assoc_BANG', $args);
}

function dissoc_BANG($hm) {
    $args = func_get_args();
    for ($i=1; $i<count($args); $i++) {
        $ktoken = $args[$i];
        unset($hm[$ktoken]);
    }
    return $hm;
}

function dissoc($src_hm) {
    $args = func_get_args();
    $hm = clone $src_hm;
    $args[0] = $hm;
    return call_user_func_array('dissoc_BANG', $args);
}

function get($hm, $k) {
    if ($hm && $hm->offsetExists($k)) {
        return $hm[$k];
    } else {
        return NULL;
    }
}

function contains_Q($hm, $k) { return array_key_exists($k, $hm); }

function keys($hm) {
    return call_user_func_array('new_list', array_keys($hm->getArrayCopy()));
}
function vals($hm) {
    return call_user_func_array('new_list', array_values($hm->getArrayCopy()));
}


// errors/exceptions
class Error extends Exception {
    public $obj = null;
    public function __construct($obj) {
        parent::__construct("Mal Error", 0, null);
        $this->obj = $obj;
    }
}

function mal_throw($obj) { throw new Error($obj); }


// lists
class ListClass extends SeqClass {
    public $meta = NULL;
}

function new_list() {
    $v = new ListClass();
    $v->exchangeArray(func_get_args());
    return $v;
}

function list_Q($obj) { return $obj instanceof ListClass; }

// vectors
class VectorClass extends SeqClass {
    public $meta = NULL;
}

function new_vector() {
    $v = new VectorClass();
    $v->exchangeArray(func_get_args());
    return $v;
}

function vector_Q($obj) { return $obj instanceof VectorClass; }


// Atoms

class Atom {
    public $value = NULL;
    public $meta = NULL;
    public function __construct($value) {
        $this->value = $value;
    }
}
function new_atom($val) { return new Atom($val); }
function atom_Q($atm) { return $atm instanceof Atom; }
function deref($atm) { return $atm->value; }
function reset_BANG($atm, $val) { return $atm->value = $val; }
function swap_BANG($atm, $f) {
    $args = array_slice(func_get_args(),2);
    array_unshift($args, $atm->value);
    $atm->value = call_user_func_array($f, $args);
    return $atm->value;
}


// Sequence operations
function sequential_Q($seq) { return list_Q($seq) or vector_Q($seq); }

function scount($seq) { return ($seq === NULL ? 0 : $seq->count()); }

function empty_Q($seq) { return $seq->count() === 0; }

function cons($a, $b) {
    $tmp = $b->getArrayCopy();
    array_unshift($tmp, $a);
    $l = new ListClass();
    $l->exchangeArray($tmp);
    return $l;
}

function concat() {
    $args = func_get_args();
    $tmp = array();
    foreach ($args as $arg) {
        $tmp = array_merge($tmp, $arg->getArrayCopy());
    }
    $l = new ListClass();
    $l->exchangeArray($tmp);
    return $l;
}

function conj($src) {
    $args = array_slice(func_get_args(), 1);
    $tmp = $src->getArrayCopy();
    foreach ($args as $arg) {
        $tmp[] = $arg;
    }
    if (list_Q($src)) {
        $s = new ListClass();
    } else {
        $s = new VectorClass();
    }
    $s->exchangeArray($tmp);
    return $s;
}

function first($seq) {
    if (count($seq) === 0) {
        return NULL;
    } else {
        return $seq[0];
    }
}

function rest($seq) {
    $l = new ListClass();
    $l->exchangeArray(array_slice($seq->getArrayCopy(), 1));
    return $l;
}

function nth($seq, $idx) {
    return $seq[$idx];
}

function apply($f, $args) {
    return $f->apply($args->getArrayCopy());
}

function map($f, $seq) {
    $l = new ListClass();
    $l->exchangeArray(array_map($f, $seq->getArrayCopy()));
    return $l;
}


// Environment
class Env {
    public $data = array();
    public $outer = NULL;
    public function __construct($outer, $binds=NULL, $exprs=NULL) {
        $this->outer = $outer;
        if ($binds) {
            if (sequential_Q($exprs)) {
                $exprs = $exprs->getArrayCopy();
            }
            for ($i=0; $i<count($binds); $i++) {
                if ($binds[$i]->value === "&") {
                    if ($exprs !== NULL && $i < count($exprs)) {
                        $lst = call_user_func_array('new_list', array_slice($exprs, $i));
                    } else {
                        $lst = new_list();
                    }
                    $this->data[$binds[$i+1]->value] = $lst;
                    break;
                } else {
                    if ($exprs !== NULL && $i < count($exprs)) {
                        $this->data[$binds[$i]->value] = $exprs[$i];
                    } else {
                        $this->data[$binds[$i]->value] = NULL;
                    }
                }
            }
        }
    }
    public function find($key) {
        if (array_key_exists($key, $this->data)) {
            return $this;
        } elseif ($this->outer) {
            return $this->outer->find($key);
        } else {
            return NULL;
        }
    }
    public function set($key, $value) {
        $this->data[$key] = $value;
        return $value;
    }
    public function get($key) {
        $env = $this->find($key);
        if (!$env) {
            throw new Exception("'" . $key . "' not found");
        } else {
            return $env->data[$key];
        }
    }
}

// types_ns is namespace of type functions
$types_ns = array(
    'pr-str'=> function () { return call_user_func_array('pr_str', func_get_args()); },
    'str'=>    function () { return call_user_func_array('str', func_get_args()); },
    'prn'=>    function () { return call_user_func_array('prn', func_get_args()); },
    'println'=>function () { return call_user_func_array('println', func_get_args()); },
    'with-meta'=> function ($a, $b) { return with_meta($a, $b); },
    'meta'=>   function ($a) { return meta($a); },
    '='=>      function ($a, $b) { return equal_Q($a, $b); },
    'nil?'=>   function ($a) { return nil_Q($a); },
    'true?'=>  function ($a) { return true_Q($a); },
    'false?'=> function ($a) { return false_Q($a); },
    '+'=>      function ($a, $b) { return intval($a + $b,10); },
    '-'=>      function ($a, $b) { return intval($a - $b,10); },
    '*'=>      function ($a, $b) { return intval($a * $b,10); },
    '/'=>      function ($a, $b) { return intval($a / $b,10); },
    '<'=>      function ($a, $b) { return $a < $b; },
    '<='=>     function ($a, $b) { return $a <= $b; },
    '>'=>      function ($a, $b) { return $a > $b; },
    '>='=>     function ($a, $b) { return $a >= $b; },
    'symbol?'=> function ($a) { return symbol_Q($a); },
    'string?'=> function ($a) { return string_Q($a); },
    'hash-map' => function () { return call_user_func_array('new_hash_map', func_get_args()); },
    'map?'=>   function ($a) { return hash_map_Q($a); },
    'assoc' => function () { return call_user_func_array('assoc', func_get_args()); },
    'dissoc' => function () { return call_user_func_array('dissoc', func_get_args()); },
    'get' =>   function ($a, $b) { return get($a, $b); },
    'contains?' => function ($a, $b) { return contains_Q($a, $b); },
    'keys' =>  function ($a) { return keys($a); },
    'vals' =>  function ($a) { return vals($a); },
    'throw'=>  function ($a) { return mal_throw($a); },
    'list'=>   function () { return call_user_func_array('new_list', func_get_args()); },
    'list?'=>  function ($a) { return list_Q($a); },
    'vector'=> function () { return call_user_func_array('new_vector', func_get_args()); },
    'vector?'=> function ($a) { return vector_Q($a); },
    'atom'=>   function ($a) { return new_atom($a); },
    'atom?'=>  function ($a) { return atom_Q($a); },
    'deref'=>  function ($a) { return deref($a); },
    'reset!'=> function ($a, $b) { return reset_BANG($a, $b); },
    'swap!'=>  function () { return call_user_func_array('swap_BANG', func_get_args()); },
    'sequential?'=> function ($a) { return sequential_Q($a); },
    'count'=>  function ($a) { return scount($a); },
    'empty?'=> function ($a) { return empty_Q($a); },
    'cons'=>   function ($a, $b) { return cons($a, $b); },
    'concat'=> function () { return call_user_func_array('concat', func_get_args()); },
    'conj'=>   function () { return call_user_func_array('conj', func_get_args()); },
    'first'=>  function ($a) { return first($a); },
    'rest'=>   function ($a) { return rest($a); },
    'nth'=>    function ($a, $b) { return nth($a, $b); },
    'apply'=>  function ($a, $b) { return apply($a, $b); },
    'map'=>    function ($a, $b) { return map($a, $b); }
);


?>
