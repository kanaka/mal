<?php

require_once 'types.php';

function _to_php($obj) {
    if (_list_Q($obj) || _vector_Q($obj) || _hash_map_Q($obj)) {
        $ret = array();
        foreach ($obj as $k => $v) {
            $ret[_to_php($k)] = _to_php($v);
        }
        return $ret;
    } elseif (is_string($obj)) {
        if (strpos($obj, chr(0x7f)) === 0) {
            return ":".substr($obj,1);
        } else {
            return $obj;
        }
    } elseif (_symbol_Q($obj)) {
        return ${$obj->value};
    } elseif (_atom_Q($obj)) {
        return $obj->value;
    } else {
        return $obj;
    }
}

function _to_mal($obj) {
    switch (gettype($obj)) {
    case "object":
      return _to_mal(get_object_vars($obj));
    case "array":
        $obj_conv = array();
        foreach ($obj as $k => $v) {
            $obj_conv[_to_mal($k)] = _to_mal($v);
        }
        if ($obj_conv !== array_values($obj_conv)) {
            $new_obj = _hash_map();
            $new_obj->exchangeArray($obj_conv);
            return $new_obj;
        } else {
            return call_user_func_array('_list', $obj_conv);
        }
    default:
        return $obj;
    }
}

?>
