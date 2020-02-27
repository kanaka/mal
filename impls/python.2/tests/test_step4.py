import unittest

import step4_if_fn_do
from env import Env
from mal_types import MalInvalidArgumentException
from mal_types import MalList, MalInt, MalFunctionCompiled, MalBoolean
from mal_types import MalSymbol


class TestStep4(unittest.TestCase):
    def test_step4_nil(self):
        self.assertEqual("nil", step4_if_fn_do.rep("nil"))

    def test_step4_boolean(self):
        self.assertEqual("true", step4_if_fn_do.rep("true"))
        self.assertEqual("false", step4_if_fn_do.rep("false"))

    def test_print_function(self):
        self.assertEqual("#<function>", str(MalFunctionCompiled(lambda x: MalInt(0))))

    def test_if_basic_true(self):
        env = Env(None)
        self.assertEqual(
            4321,
            step4_if_fn_do.EVAL(
                MalList(
                    [MalSymbol("if"), MalBoolean(True), MalInt(4321), MalInt(1234)]
                ),
                env,
            ).native(),
        )

    def test_if_basic_false(self):
        env = Env(None)
        self.assertEqual(
            1234,
            step4_if_fn_do.EVAL(
                MalList(
                    [MalSymbol("if"), MalBoolean(False), MalInt(4321), MalInt(1234)]
                ),
                env,
            ).native(),
        )

    def test_if_basic_false_no_fourth_arg(self):
        env = Env(None)
        self.assertEqual(
            "nil",
            str(
                step4_if_fn_do.EVAL(
                    MalList([MalSymbol("if"), MalBoolean(False), MalInt(4321)]), env
                )
            ),
        )

    def test_env_constructor_binds(self):
        env = Env(outer=None, binds=[MalSymbol("a")], exprs=[MalInt(3)])
        self.assertEqual(3, env.get("a").native())

    def test_env_constructor_binds_multiple(self):
        env = Env(
            outer=None,
            binds=[MalSymbol("a"), MalSymbol("b")],
            exprs=[MalInt(44), MalInt(32)],
        )
        self.assertEqual(44, env.get("a").native())
        self.assertEqual(32, env.get("b").native())

    def test_step4_do(self):
        self.assertEqual("44", step4_if_fn_do.rep("(do 1 2 3 44)"))
        self.assertEqual("21", step4_if_fn_do.rep("(do 21)"))

    def test_step4_fn(self):
        self.assertEqual("#<function>", step4_if_fn_do.rep("(fn* (a) 0)"))

    def test_step4_use_fn(self):
        self.assertEqual("7", step4_if_fn_do.rep("((fn* (a) a) 7)"))

    def test_step4_use_fn_multiple(self):
        self.assertEqual("8", step4_if_fn_do.rep("((fn* (a b) a) 8 9)"))

    def test_step4_use_fn_multiple_nested(self):
        self.assertEqual("10", step4_if_fn_do.rep("((fn* (a b) (+ a (+ b 1))) 4 5)"))

    def test_step4_use_fn_func_param(self):
        self.assertEqual(
            "8", step4_if_fn_do.rep("((fn* (f x) (f x)) (fn* (a) (+ 1 a)) 7)")
        )

    def test_step4_prn(self):
        self.assertEqual("nil", step4_if_fn_do.rep("(prn 4)"))

    def test_step4_list(self):
        self.assertEqual("(1 2 (3 4) 5)", step4_if_fn_do.rep("(list 1 2 (list 3 4) 5)"))

    def test_step4_listP(self):
        self.assertEqual("true", step4_if_fn_do.rep("(list? (list 1 2))"))
        self.assertEqual("false", step4_if_fn_do.rep("(list? 4)"))

    def test_step4_empty(self):
        self.assertEqual("true", step4_if_fn_do.rep("(empty? (list))"))

    def test_step4_count(self):
        self.assertEqual("0", step4_if_fn_do.rep("(count (list))"))
        self.assertEqual("2", step4_if_fn_do.rep("(count (list 1 2))"))
        self.assertEqual("0", step4_if_fn_do.rep("(count nil)"))

    def test_step4_equal(self):
        self.assertEqual("true", step4_if_fn_do.rep("(= 0 0)"))
        self.assertEqual("true", step4_if_fn_do.rep("(= (list 1) (list 1))"))
        self.assertEqual("false", step4_if_fn_do.rep("(= (list 1) (list 1 2))"))
        self.assertEqual(
            "true",
            step4_if_fn_do.rep("(= (list (list 1) (list 2)) (list (list 1) (list 2)))"),
        )
        self.assertEqual("true", step4_if_fn_do.rep("(= nil nil)"))

    def test_step4_less(self):
        self.assertEqual("true", step4_if_fn_do.rep("(< 1 2)"))
        self.assertEqual("false", step4_if_fn_do.rep("(< 2 1)"))
        self.assertEqual("false", step4_if_fn_do.rep("(< 1 1)"))
        try:
            step4_if_fn_do.rep("(< 1 nil)")
            self.fail("Expected exception")
        except MalInvalidArgumentException:
            pass
        try:
            step4_if_fn_do.rep("(< nil 1)")
            self.fail("Expected exception")
        except MalInvalidArgumentException:
            pass

    def test_step4_less_equal(self):
        self.assertEqual("true", step4_if_fn_do.rep("(<= 1 2)"))
        self.assertEqual("false", step4_if_fn_do.rep("(<= 2 1)"))
        self.assertEqual("true", step4_if_fn_do.rep("(<= 1 1)"))
        try:
            step4_if_fn_do.rep("(<= 1 nil)")
            self.fail("Expected exception")
        except MalInvalidArgumentException:
            pass
        try:
            step4_if_fn_do.rep("(<= nil 1)")
            self.fail("Expected exception")
        except MalInvalidArgumentException:
            pass

    def test_step4_more(self):
        self.assertEqual("false", step4_if_fn_do.rep("(> 1 2)"))
        self.assertEqual("true", step4_if_fn_do.rep("(> 2 1)"))
        self.assertEqual("false", step4_if_fn_do.rep("(> 1 1)"))
        try:
            step4_if_fn_do.rep("(> 1 nil)")
            self.fail("Expected exception")
        except MalInvalidArgumentException:
            pass
        try:
            step4_if_fn_do.rep("(> nil 1)")
            self.fail("Expected exception")
        except MalInvalidArgumentException:
            pass

    def test_step4_more_equal(self):
        self.assertEqual("false", step4_if_fn_do.rep("(>= 1 2)"))
        self.assertEqual("true", step4_if_fn_do.rep("(>= 2 1)"))
        self.assertEqual("true", step4_if_fn_do.rep("(>= 1 1)"))
        try:
            step4_if_fn_do.rep("(>= 1 nil)")
            self.fail("Expected exception")
        except MalInvalidArgumentException:
            pass
        try:
            step4_if_fn_do.rep("(>= nil 1)")
            self.fail("Expected exception")
        except MalInvalidArgumentException:
            pass

    def test_step4_closures(self):
        self.assertEqual(
            "12", step4_if_fn_do.rep("(( (fn* (a) (fn* (b) (+ a b))) 5) 7)")
        )
        self.assertEqual(
            "#<function>",
            step4_if_fn_do.rep("(def! gen-plus5 (fn* () (fn* (b) (+ 5 b))))"),
        )
        self.assertEqual(
            "#<function>",
            step4_if_fn_do.rep("(def! gen-plus5 (fn* () (fn* (b) (+ 5 b))))"),
        )
        self.assertEqual("#<function>", step4_if_fn_do.rep("(def! plus5 (gen-plus5))"))
        self.assertEqual("12", step4_if_fn_do.rep("(plus5 7)"))

    def test_step4_variadic_a(self):
        self.assertEqual(
            "3", step4_if_fn_do.rep("( (fn* (& more) (count more)) 1 2 3)")
        )

    def test_step4_variadic_b(self):
        self.assertEqual("0", step4_if_fn_do.rep("((fn* (& more) (count more)))"))

    def test_step4_quoted_string(self):
        self.assertEqual('"\\""', step4_if_fn_do.rep('"\\""'))

    def test_step4_str(self):
        self.assertEqual('"(1 a 2 3)"', step4_if_fn_do.rep('(str (list 1 "a" 2 3))'))

    def test_step4_equal_vector_list(self):
        self.assertEqual("true", step4_if_fn_do.rep("(=[] (list))"))


if __name__ == "__main__":
    unittest.main()
