<?php

require_once 'types.php';

function _pr_str($obj, $print_readably=True) {
    if (_list_Q($obj)) {
        $ret = array();
        foreach ($obj as $e) {
            array_push($ret, _pr_str($e, $print_readably));
        }
        return "(" . implode(" ", $ret) . ")";
    } elseif (_vector_Q($obj)) {
        $ret = array();
        foreach ($obj as $e) {
            array_push($ret, _pr_str($e, $print_readably));
        }
        return "[" . implode(" ", $ret) . "]";
    } elseif (_hash_map_Q($obj)) {
        $ret = array();
        foreach (array_keys($obj->getArrayCopy()) as $k) {
            $ret[] = _pr_str($k, $print_readably);
            $ret[] = _pr_str($obj[$k], $print_readably);
        }
        return "{" . implode(" ", $ret) . "}";
    } elseif (is_string($obj)) {
        if (strpos($obj, chr(0x7f)) === 0) {
            return ":".substr($obj,1);
        } elseif ($print_readably) {
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
    } elseif (_symbol_Q($obj)) {
        return $obj->value;
    } elseif (_atom_Q($obj)) {
        return "(atom " . _pr_str($obj->value, $print_readably) . ")";
    } elseif (_function_Q($obj)) {
        return "(fn* [...] ...)";
    } elseif (is_callable($obj)) {  // only step4 and below
        return "#<function ...>";
    } else {
        throw new Exception("_pr_str unknown type: " . gettype($obj));
    }
}

?>
