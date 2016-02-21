<?php


// Errors/Exceptions
class Error extends Exception {
    public $obj = null;
    public function __construct($obj) {
        parent::__construct("Mal Error", 0, null);
        $this->obj = $obj;
    }
}


// General functions

function _equal_Q($a, $b) {
    $ota = gettype($a) === "object" ? get_class($a) : gettype($a);
    $otb = gettype($b) === "object" ? get_class($b) : gettype($b);
    if (!($ota === $otb or (_sequential_Q($a) and _sequential_Q($b)))) {
        return false;
    } elseif (_symbol_Q($a)) {
        #print "ota: $ota, otb: $otb\n";
        return $a->value === $b->value;
    } elseif (_list_Q($a) or _vector_Q($a)) {
        if ($a->count() !== $b->count()) { return false; }
        for ($i=0; $i<$a->count(); $i++) {
            if (!_equal_Q($a[$i], $b[$i])) { return false; }
        }
        return true;
    } elseif (_hash_map_Q($a)) {
        if ($a->count() !== $b->count()) { return false; }
        $hm1 = $a->getArrayCopy();
        $hm2 = $b->getArrayCopy();
        foreach (array_keys($hm1) as $k) {
            if (!_equal_Q($hm1[$k], $hm2[$k])) { return false; }
        }
        return true;
    } else {
        return $a === $b;
    }
}

function _sequential_Q($seq) { return _list_Q($seq) or _vector_Q($seq); }


// Scalars
function _nil_Q($obj) { return $obj === NULL; }
function _true_Q($obj) { return $obj === true; }
function _false_Q($obj) { return $obj === false; }
function _string_Q($obj) { return is_string($obj); }


// Symbols
class SymbolClass {
    public $value = NULL;
    public $meta = NULL;
    public function __construct($value) {
        $this->value = $value;
    }
}
function _symbol($name) { return new SymbolClass($name); }
function _symbol_Q($obj) { return ($obj instanceof SymbolClass); }

// Keywords
function _keyword($name) { return chr(0x7f).$name; }
function _keyword_Q($obj) {
    return is_string($obj) && strpos($obj, chr(0x7f)) === 0;
}



// Functions
class FunctionClass {
    public $func = NULL;
    public $type = 'native';   // 'native' or 'platform'
    public $meta = NULL;
    public $ast = NULL;
    public $env = NULL;
    public $params = NULL;
    public $ismacro = False;
    public function __construct($func, $type,
                                $ast, $env, $params, $ismacro=False) {
        $this->func = $func;
        $this->type = $type;
        $this->ast = $ast;
        #print_r($ast);
        $this->env = $env;
        $this->params = $params;
        $this->ismacro = $ismacro;
    }
    public function __invoke() {
        $args = func_get_args();
        if ($this->type === 'native') {
            $fn_env = new Env($this->env,
                              $this->params, $args);
            $evalf = $this->func;
            return $evalf($this->ast, $fn_env);
        } else {
            return call_user_func_array($this->func, $args);
        }
    }
    public function gen_env($args) {
        return new Env($this->env, $this->params, $args);
    }
    public function apply($args) {
        return call_user_func_array(array(&$this, '__invoke'),$args);
    }
}

function _function($func, $type='platform',
                   $ast=NULL, $env=NULL, $params=NULL, $ismacro=False) {
    return new FunctionClass($func, $type, $ast, $env, $params, $ismacro);
}
function _function_Q($obj) { return $obj instanceof FunctionClass; }


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


// Lists
class ListClass extends SeqClass {
    public $meta = NULL;
}

function _list() {
    $v = new ListClass();
    $v->exchangeArray(func_get_args());
    return $v;
}
function _list_Q($obj) { return $obj instanceof ListClass; }


// Vectors
class VectorClass extends SeqClass {
    public $meta = NULL;
}

function _vector() {
    $v = new VectorClass();
    $v->exchangeArray(func_get_args());
    return $v;
}
function _vector_Q($obj) { return $obj instanceof VectorClass; }


// Hash Maps
class HashMapClass extends ArrayObject {
    public $meta = NULL;
}

function _hash_map() {
    $args = func_get_args();
    if (count($args) % 2 === 1) {
        throw new Exception("Odd number of hash map arguments");
    }
    $hm = new HashMapClass();
    array_unshift($args, $hm);
    return call_user_func_array('_assoc_BANG', $args);
}
function _hash_map_Q($obj) { return $obj instanceof HashMapClass; }

function _assoc_BANG($hm) {
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

function _dissoc_BANG($hm) {
    $args = func_get_args();
    for ($i=1; $i<count($args); $i++) {
        $ktoken = $args[$i];
        if ($hm && $hm->offsetExists($ktoken)) {
            unset($hm[$ktoken]);
        }
    }
    return $hm;
}


// Atoms
class Atom {
    public $value = NULL;
    public $meta = NULL;
    public function __construct($value) {
        $this->value = $value;
    }
}
function _atom($val) { return new Atom($val); }
function _atom_Q($atm) { return $atm instanceof Atom; }

?>
