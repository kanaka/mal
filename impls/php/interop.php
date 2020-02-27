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

function _to_native($name, $env) {
  if (is_callable($name)) {
    return _function(function() use ($name) {
      $args = array_map("_to_php", func_get_args());
      $res = call_user_func_array($name, $args);
      return _to_mal($res);
    });
  // special case for language constructs
  } else if ($name == "print") {
    return _function(function($value) {
      print(_to_php($value));
      return null;
    });
  } else if ($name == "exit") {
    return _function(function($value) {
      exit(_to_php($value));
      return null;
    });
  } else if ($name == "require") {
    return _function(function($value) {
      require(_to_php($value));
      return null;
    });
  } else if (in_array($name, ["_SERVER", "_GET", "_POST", "_FILES", "_REQUEST", "_SESSION", "_ENV", "_COOKIE"])) {
      $val = $GLOBALS[$name];
  } else if (defined($name)) {
      $val = constant($name);
  } else {
      $val = ${$name};
  }
  return _to_mal($val);
}
?>
