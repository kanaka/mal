<?php

require_once 'types.php';
require_once 'readline.php';
require_once 'reader.php';
require_once 'printer.php';

// Error/Exception functions
function mal_throw($obj) { throw new Error($obj); }


// String functions
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


// Number functions
function time_ms() {
    return intval(microtime(1) * 1000);
}


// Hash Map functions
function assoc($src_hm) {
    $args = func_get_args();
    $hm = clone $src_hm;
    $args[0] = $hm;
    return call_user_func_array('_assoc_BANG', $args);
}

function dissoc($src_hm) {
    $args = func_get_args();
    $hm = clone $src_hm;
    $args[0] = $hm;
    return call_user_func_array('_dissoc_BANG', $args);
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
    return call_user_func_array('_list', array_keys($hm->getArrayCopy()));
}
function vals($hm) {
    return call_user_func_array('_list', array_values($hm->getArrayCopy()));
}


// Sequence functions
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

function nth($seq, $idx) {
    if ($idx < $seq->count()) {
        return $seq[$idx];
    } else {
        throw new Exception("nth: index out of range");
    }
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

function empty_Q($seq) { return $seq->count() === 0; }

function scount($seq) { return ($seq === NULL ? 0 : $seq->count()); }

function conj($src) {
    $args = array_slice(func_get_args(), 1);
    $tmp = $src->getArrayCopy();
    if (_list_Q($src)) {
        foreach ($args as $arg) { array_unshift($tmp, $arg); }
        $s = new ListClass();
    } else {
        foreach ($args as $arg) { $tmp[] = $arg; }
        $s = new VectorClass();
    }
    $s->exchangeArray($tmp);
    return $s;
}

function apply($f) {
    $args = array_slice(func_get_args(), 1);
    $last_arg = array_pop($args)->getArrayCopy();
    return $f->apply(array_merge($args, $last_arg));
}

function map($f, $seq) {
    $l = new ListClass();
    $l->exchangeArray(array_map($f, $seq->getArrayCopy()));
    return $l;
}


// Metadata functions
function with_meta($obj, $m) {
    $new_obj = clone $obj;
    $new_obj->meta = $m;
    return $new_obj;
}

function meta($obj) {
    return $obj->meta;
}


// Atom functions
function deref($atm) { return $atm->value; }
function reset_BANG($atm, $val) { return $atm->value = $val; }
function swap_BANG($atm, $f) {
    $args = array_slice(func_get_args(),2);
    array_unshift($args, $atm->value);
    $atm->value = call_user_func_array($f, $args);
    return $atm->value;
}


// core_ns is namespace of type functions
$core_ns = array(
    '='=>      function ($a, $b) { return _equal_Q($a, $b); },
    'throw'=>  function ($a) { return mal_throw($a); },
    'nil?'=>   function ($a) { return _nil_Q($a); },
    'true?'=>  function ($a) { return _true_Q($a); },
    'false?'=> function ($a) { return _false_Q($a); },
    'symbol'=> function () { return call_user_func_array('_symbol', func_get_args()); },
    'symbol?'=> function ($a) { return _symbol_Q($a); },
    'keyword'=> function () { return call_user_func_array('_keyword', func_get_args()); },
    'keyword?'=> function ($a) { return _keyword_Q($a); },

    'string?'=> function ($a) { return _string_Q($a); },
    'pr-str'=> function () { return call_user_func_array('pr_str', func_get_args()); },
    'str'=>    function () { return call_user_func_array('str', func_get_args()); },
    'prn'=>    function () { return call_user_func_array('prn', func_get_args()); },
    'println'=>function () { return call_user_func_array('println', func_get_args()); },
    'readline'=>function ($a) { return mal_readline($a); },
    'read-string'=>function ($a) { return read_str($a); },
    'slurp'=>  function ($a) { return file_get_contents($a); },
    '<'=>      function ($a, $b) { return $a < $b; },
    '<='=>     function ($a, $b) { return $a <= $b; },
    '>'=>      function ($a, $b) { return $a > $b; },
    '>='=>     function ($a, $b) { return $a >= $b; },
    '+'=>      function ($a, $b) { return intval($a + $b,10); },
    '-'=>      function ($a, $b) { return intval($a - $b,10); },
    '*'=>      function ($a, $b) { return intval($a * $b,10); },
    '/'=>      function ($a, $b) { return intval($a / $b,10); },
    'time-ms'=>function () { return time_ms(); },

    'list'=>   function () { return call_user_func_array('_list', func_get_args()); },
    'list?'=>  function ($a) { return _list_Q($a); },
    'vector'=> function () { return call_user_func_array('_vector', func_get_args()); },
    'vector?'=> function ($a) { return _vector_Q($a); },
    'hash-map' => function () { return call_user_func_array('_hash_map', func_get_args()); },
    'map?'=>   function ($a) { return _hash_map_Q($a); },
    'assoc' => function () { return call_user_func_array('assoc', func_get_args()); },
    'dissoc' => function () { return call_user_func_array('dissoc', func_get_args()); },
    'get' =>   function ($a, $b) { return get($a, $b); },
    'contains?' => function ($a, $b) { return contains_Q($a, $b); },
    'keys' =>  function ($a) { return keys($a); },
    'vals' =>  function ($a) { return vals($a); },

    'sequential?'=> function ($a) { return _sequential_Q($a); },
    'cons'=>   function ($a, $b) { return cons($a, $b); },
    'concat'=> function () { return call_user_func_array('concat', func_get_args()); },
    'nth'=>    function ($a, $b) { return nth($a, $b); },
    'first'=>  function ($a) { return first($a); },
    'rest'=>   function ($a) { return rest($a); },
    'empty?'=> function ($a) { return empty_Q($a); },
    'count'=>  function ($a) { return scount($a); },
    'conj'=>   function () { return call_user_func_array('conj', func_get_args()); },
    'apply'=>  function () { return call_user_func_array('apply', func_get_args()); },
    'map'=>    function ($a, $b) { return map($a, $b); },

    'with-meta'=> function ($a, $b) { return with_meta($a, $b); },
    'meta'=>   function ($a) { return meta($a); },
    'atom'=>   function ($a) { return _atom($a); },
    'atom?'=>  function ($a) { return _atom_Q($a); },
    'deref'=>  function ($a) { return deref($a); },
    'reset!'=> function ($a, $b) { return reset_BANG($a, $b); },
    'swap!'=>  function () { return call_user_func_array('swap_BANG', func_get_args()); },
);


?>
