import unittest

import core
import step8_macros
from env import Env
from mal_types import MalFunctionCompiled, MalInt, MalFunctionRaw, MalList
from mal_types import MalInvalidArgumentException, MalIndexError


class TestStep8(unittest.TestCase):
    def setUp(self) -> None:
        self._repl_env = step8_macros.init_repl_env()

    def rep(self, input: str) -> str:
        return step8_macros.rep(input, self._repl_env)

    def test_step8_is_macro(self):
        self.assertEqual(False, MalFunctionCompiled(lambda a: MalInt(1)).is_macro())
        self.assertEqual(
            False,
            MalFunctionRaw(core.ns["+"], MalInt(1), MalList([]), Env(None)).is_macro(),
        )

    def test_step8_defmacro(self):
        self.assertEqual("#<macro>", self.rep("(defmacro! one (fn* () 1))"))

    def test_step8_quote_reader_macro(self):
        self.assertEqual("(+ 1 2)", self.rep("'(+ 1 2)"))

    def test_step8_quasiquote_unquote_reader_macros(self):
        self.assertEqual("(+ 1 3)", self.rep("`(+ 1 ~(+ 1 2))"))

    def test_step8_repl_env_isolation(self):
        env1 = step8_macros.init_repl_env()
        step8_macros.rep("(def! a 2)", env1)
        env2 = step8_macros.init_repl_env()
        step8_macros.rep("(def! a 3)", env2)
        self.assertEqual("2", step8_macros.rep("a", env1))
        self.assertEqual("3", step8_macros.rep("a", env2))
        self.assertEqual("6", step8_macros.rep("(eval (list + a 3))", env2))

    def test_step8_is_macro_call(self):
        self.rep("(defmacro! macro (fn* () 1))")
        self.rep("(def! func (fn* () 1))")
        self.rep("(def! q 4)")
        macro = step8_macros.READ("(macro)")
        func = step8_macros.READ("(func)")
        other1 = step8_macros.READ("(x)")
        other2 = step8_macros.READ("(1)")
        other3 = step8_macros.READ("(2)")
        other4 = step8_macros.READ("(q)")
        self.assertTrue(step8_macros.is_macro_call(macro, self._repl_env))
        self.assertFalse(step8_macros.is_macro_call(func, self._repl_env))
        self.assertFalse(step8_macros.is_macro_call(other1, self._repl_env))
        self.assertFalse(step8_macros.is_macro_call(other2, self._repl_env))
        self.assertFalse(step8_macros.is_macro_call(other3, self._repl_env))
        self.assertFalse(step8_macros.is_macro_call(other4, self._repl_env))

    def test_step8_macroexpand(self):
        self.rep("(def! func (fn* () 1))")
        func = step8_macros.READ("(func)")
        self.assertEqual("(func)", str(step8_macros.macroexpand(func, self._repl_env)))
        self.rep("(defmacro! macro (fn* () 1))")
        macro = step8_macros.READ("(macro)")
        self.assertEqual("1", str(step8_macros.macroexpand(macro, self._repl_env)))
        self.rep("(defmacro! unless (fn* (pred a b) `(if ~pred ~b ~a)))")
        self.assertEqual("(if true 7 8)", self.rep("(macroexpand (unless true 8 7))"))

    def test_step8_not(self):
        self.assertEqual("true", self.rep("(not (not true))"))
        self.assertEqual("true", self.rep("(not nil)"))
        self.assertEqual("false", self.rep("(not 1)"))
        self.assertEqual("true", self.rep("(not false)"))

    def test_step8_let(self):
        self.assertEqual("2", self.rep("(let* (a 1 b 2) b)"))

    def test_step8_first(self):
        self.assertEqual("2", self.rep("(first (list 2 3 4))"))
        self.assertEqual("nil", self.rep("(first (list))"))
        self.assertEqual("nil", self.rep("(first nil)"))
        with self.assertRaises(MalInvalidArgumentException):
            self.rep("(first 1)")

    def test_step8_rest(self):
        self.assertEqual("(2 3)", self.rep("(rest (list 1 2 3))"))
        self.assertEqual("()", self.rep("(rest (list))"))
        self.assertEqual("()", self.rep("(rest nil)"))
        with self.assertRaises(MalInvalidArgumentException):
            self.rep("(rest 1)")

    def test_step8_nth(self):
        self.assertEqual("3", self.rep("(nth '(1 2 3) 2)"))

        with self.assertRaises(MalIndexError):
            self.rep("(nth () 1)")


if __name__ == "__main__":
    unittest.main()
