<?php

require_once 'types.php';

class Env {
    public $data = array();
    public $outer = NULL;
    public function __construct($outer, $binds=NULL, $exprs=NULL) {
        $this->outer = $outer;
        if ($binds) {
            if (_sequential_Q($exprs)) {
                $exprs = $exprs->getArrayCopy();
            }
            for ($i=0; $i<count($binds); $i++) {
                if ($binds[$i]->value === "&") {
                    if ($exprs !== NULL && $i < count($exprs)) {
                        $lst = call_user_func_array('_list', array_slice($exprs, $i));
                    } else {
                        $lst = _list();
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
        if (array_key_exists($key->value, $this->data)) {
            return $this;
        } elseif ($this->outer) {
            return $this->outer->find($key);
        } else {
            return NULL;
        }
    }
    public function set($key, $value) {
        $this->data[$key->value] = $value;
        return $value;
    }
    public function get($key) {
        $env = $this->find($key);
        if (!$env) {
            throw new Exception("'" . $key->value . "' not found");
        } else {
            return $env->data[$key->value];
        }
    }
}

?>
