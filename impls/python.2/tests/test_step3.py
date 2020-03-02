import unittest

import mal_types
import step3_env
from env import Env
from mal_types import MalList, MalInt
from mal_types import MalSymbol
from mal_types import MalUnknownSymbolException, MalInvalidArgumentException


class TestStep3(unittest.TestCase):
    def test_env_find(self):
        e = Env(None)
        e.set("key", MalInt(1))
        result = e.find("key")
        self.assertTrue(e is result)

    def test_env_find_outer(self):
        outer = Env(None)
        e = Env(outer)
        outer.set("key", MalInt(1))
        result = e.find("key")
        self.assertTrue(result is outer)

    def test_env_find_no_key(self):
        e = Env(None)
        self.assertEqual(None, e.find("key"))

    def test_env_get(self):
        env = Env(None)
        expression = MalInt(1)
        env.set("key", expression)
        self.assertTrue(env.get("key") is expression)

    def test_env_get_error(self):
        env = Env(None)
        try:
            env.get("key")
            self.fail("Expected an exeception")
        except MalUnknownSymbolException:
            pass

    def test_MalFunctionCompiled(self):
        self.assertEqual(
            "3",
            str(
                mal_types.MalFunctionCompiled(
                    lambda a: MalInt(a[0].native() + a[1].native())
                ).call([mal_types.MalInt(1), mal_types.MalInt(2)])
            ),
        )

    def test_eval_invalid(self):
        with self.assertRaises(MalInvalidArgumentException):
            step3_env.EVAL(MalList([MalInt(1), MalInt(2)]), Env(None))

    def test_eval_1_plus_1(self):
        env = Env(None)
        env.set(
            "+",
            mal_types.MalFunctionCompiled(
                lambda a: MalInt(a[0].native() + a[1].native())
            ),
        )
        self.assertEqual(
            2,
            step3_env.EVAL(
                MalList([MalSymbol("+"), MalInt(1), MalInt(1)]), env
            ).native(),
        )

    def test_def(self):
        env = Env(None)
        self.assertEqual(
            1,
            step3_env.EVAL(
                MalList([MalSymbol("def!"), MalSymbol("a"), MalInt(1)]), env
            ).native(),
        )
        self.assertEqual(1, env.get("a").native())

    def test_mallist_native(self):
        x = MalInt(1)
        self.assertEqual([x], MalList([x]).native())

    def test_let_basic(self):
        env = Env(None)
        self.assertEqual(
            2,
            step3_env.EVAL(
                MalList(
                    [
                        MalSymbol("let*"),
                        MalList([MalSymbol("c"), MalInt(2)]),
                        MalSymbol("c"),
                    ]
                ),
                env,
            ).native(),
        )

    def test_let_advanced(self):
        env = Env(None)
        env.set(
            "+",
            mal_types.MalFunctionCompiled(
                lambda a: MalInt(a[0].native() + a[1].native())
            ),
        )
        self.assertEqual(
            4,
            step3_env.EVAL(
                MalList(
                    [
                        MalSymbol("let*"),
                        MalList([MalSymbol("c"), MalInt(2)]),
                        MalList([MalSymbol("+"), MalSymbol("c"), MalInt(2)]),
                    ]
                ),
                env,
            ).native(),
        )

    def test_let_multiple(self):
        env = Env(None)
        env.set(
            "+",
            mal_types.MalFunctionCompiled(
                lambda a: MalInt(a[0].native() + a[1].native())
            ),
        )
        self.assertEqual(
            5,
            step3_env.EVAL(
                MalList(
                    [
                        MalSymbol("let*"),
                        MalList([MalSymbol("c"), MalInt(2), MalSymbol("d"), MalInt(3)]),
                        MalList([MalSymbol("+"), MalSymbol("c"), MalSymbol("d")]),
                    ]
                ),
                env,
            ).native(),
        )

    def test_step3_let_multiple(self):
        self.assertEqual("5", step3_env.rep("(let* (c 2 d 3) (+ c d))"))

    def test_step3_let_nested_backref(self):
        self.assertEqual("6", step3_env.rep("(let* (c 2 d c) (+ c (+ d 2)))"))


if __name__ == "__main__":
    unittest.main()
