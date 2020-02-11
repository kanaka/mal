import unittest

import step9_try
from mal_types import MalException, MalIndexError, MalInvalidArgumentException


class TestStep9(unittest.TestCase):
    def setUp(self) -> None:
        self._repl_env = step9_try.init_repl_env()

    def rep(self, input: str) -> str:
        return step9_try.rep(input, self._repl_env)

    def test_step9_throw(self):
        with self.assertRaises(MalException):
            self.assertEqual("foo", self.rep('(throw "err1")'))

    def test_step9_try_catch(self):
        self.assertEqual("123", self.rep("(try* 123 (catch* e 456))"))
        self.assertEqual(
            "nil", self.rep('(try* (abc 1 2) (catch* exc (prn "exc is:" exc)))')
        )

    def test_step9_nth(self):
        self.assertEqual("3", self.rep("(nth '(1 2 3) 2)"))

        with self.assertRaises(MalIndexError):
            self.rep("(nth () 1)")

    def test_step9_apply(self):
        self.assertEqual("(1 1)", self.rep("(apply list '(1 1))"))
        self.assertEqual("(1 2 1 2)", self.rep("(apply list 1 2 '(1 2))"))

    def test_step9_map(self):
        self.assertEqual("((1) (2))", self.rep("(map list '(1 2))"))

    def test_step9_symbol_q(self):
        self.assertEqual("true", self.rep("(symbol? 'x)"))
        self.assertEqual("false", self.rep("(symbol? nil)"))

    def test_step9_nil(self):
        self.assertEqual("true", self.rep("(nil? nil)"))
        self.assertEqual("false", self.rep("(nil? 1)"))

    def test_step9_true(self):
        self.assertEqual("true", self.rep("(true? true)"))
        self.assertEqual("false", self.rep("(true? false)"))
        self.assertEqual("false", self.rep("(true? nil)"))
        self.assertEqual("false", self.rep("(true? 1)"))

    def test_step9_false(self):
        self.assertEqual("true", self.rep("(false? false)"))
        self.assertEqual("false", self.rep("(false? true)"))
        self.assertEqual("false", self.rep("(false? nil)"))
        self.assertEqual("false", self.rep("(false? 1)"))

    def test_step9_throw_hash_map(self):
        with self.assertRaises(MalException):
            self.rep('(throw {:msg "err2"})')

    def test_step9_symbol(self):
        self.assertEqual("abc", self.rep('(symbol "abc")'))

    def test_step9_complex_apply(self):
        self.assertEqual("9", self.rep("(apply + 4 [5])"))

    def test_step9_get(self):
        self.assertEqual("nil", self.rep('(get nil "a")'))
        self.assertEqual("nil", self.rep('(get (hash-map) "a")'))

    def test_step9_complex_str(self):
        self.assertEqual('"A{:abc val}Z"', self.rep('(str "A" {:abc "val"} "Z")'))

    def test_step9_sequential_q(self):
        self.assertEqual("true", self.rep("(sequential? (list 1 2 3))"))
        self.assertEqual("true", self.rep("(sequential? ())"))
        self.assertEqual("false", self.rep("(sequential? nil)"))
        self.assertEqual("false", self.rep("(sequential? 1)"))
        self.assertEqual("true", self.rep("(sequential? [1 2 3])"))
        self.assertEqual("true", self.rep("(sequential? [])"))
        self.assertEqual("false", self.rep("(sequential? {})"))

    def test_step9_vector(self):
        self.assertEqual("[1 2 3]", self.rep("(vector 1 2 3)"))
        self.assertEqual("[]", self.rep("(vector)"))
        self.assertEqual("[[1 2]]", self.rep("(vector [1 2])"))
        self.assertEqual("[nil]", self.rep("(vector nil)"))

    def test_step9_hash_map(self):
        self.assertEqual("{}", self.rep("(hash-map)"))
        self.assertEqual('{"a" 1}', self.rep('(hash-map "a" 1)'))
        self.assertEqual('{"a" 1 "b" 2}', self.rep('(hash-map "a" 1 "b" 2)'))

    def test_step9_assoc(self):
        with self.assertRaises(MalInvalidArgumentException):
            self.rep("(assoc)")
        self.assertEqual("1", self.rep("(assoc 1)"))
        self.assertEqual("nil", self.rep("(assoc nil)"))
        self.assertEqual("{}", self.rep("(assoc {})"))
        self.assertEqual('{"a" 1}', self.rep('(assoc {} "a" 1)'))
        self.assertEqual('{"b" 2 "a" 1}', self.rep('(assoc {"b" 2} "a" 1)'))
        self.assertEqual('{"b" 2 "a" 1 "c" 3}', self.rep('(assoc {"b" 2} "a" 1 "c" 3)'))
        self.assertEqual('{"b" 3}', self.rep('(assoc {"b" 2} "b" 3)'))
        self.assertEqual("{:bcd 234}", self.rep("(assoc {} :bcd 234)"))

    def test_step9_contains_q(self):
        with self.assertRaises(MalInvalidArgumentException):
            self.rep("(contains?)")
        with self.assertRaises(MalInvalidArgumentException):
            self.rep("(contains? 1)")
        with self.assertRaises(MalInvalidArgumentException):
            self.rep("(contains? nil)")
        with self.assertRaises(MalInvalidArgumentException):
            self.rep("(contains? nil nil)")
        self.assertEqual("false", self.rep("(contains? {} nil)"))
        self.assertEqual("true", self.rep('(contains? {"a" 1} "a")'))
        self.assertEqual("true", self.rep('(contains? {"a" 1 :b 2} :b)'))

    def test_step9_keys(self):
        with self.assertRaises(MalInvalidArgumentException):
            self.rep("(keys)")
        with self.assertRaises(MalInvalidArgumentException):
            self.rep("(keys 1)")
        self.assertEqual('("a")', self.rep('(keys {"a" 1})'))
        self.assertEqual('("a" :b)', self.rep('(keys {"a" 1 :b 2})'))

    def test_step9_vals(self):
        with self.assertRaises(MalInvalidArgumentException):
            self.rep("(vals)")
        with self.assertRaises(MalInvalidArgumentException):
            self.rep("(vals 1)")
        self.assertEqual("(1)", self.rep('(vals {"a" 1})'))
        self.assertEqual("(1 2)", self.rep('(vals {"a" 1 :b 2})'))

    def test_step9_dissoc(self):
        self.assertEqual('{"c" 3}', self.rep('(dissoc {"a" 1 "b" 2 "c" 3} "a" "b")'))
        self.assertEqual(
            '{"c" 3}', self.rep('(dissoc {"a" 1 "b" 2 "c" 3} "a" "b" "d")')
        )


if __name__ == "__main__":
    unittest.main()
