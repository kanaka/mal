import unittest

import stepA_mal


class TestStepA(unittest.TestCase):
    def setUp(self) -> None:
        self._repl_env = stepA_mal.init_repl_env()

    def rep(self, input: str) -> str:
        return stepA_mal.rep(input, self._repl_env)

    def test_stepA_host_language(self):
        self.assertEqual('"python.2"', self.rep("*host-language*"))

    def test_stepA_eval_vector(self):
        self.assertEqual("[1 2 3]", self.rep("[1 2 (+ 1 2)]"))

    def test_reader_multiple_lines(self):
        self.assertEqual("3", self.rep("(do\n1\n2\n3\n)"))

    def test_read_string_multiple_lines(self):
        self.assertEqual(
            "(do 2 nil)",
            self.rep('(read-string (str "(do \n" ";; read\n" "2\n" "\n nil)"))'),
        )

    def test_read_hash_map(self):
        self.assertEqual("{}", self.rep("{}"))
        self.assertEqual('{"a" 1}', self.rep('{"a" 1}'))
        self.assertEqual('{"1" 2 "3" 4}', self.rep('{"1" 2 "3" 4}'))

    def test_get(self):
        self.assertEqual("1", self.rep('(get {"+" 1} "+")'))

    def test_keyword(self):
        self.assertEqual(":keyword", self.rep(":keyword"))

    def test_deref_reader_macro(self):
        self.assertEqual("1", self.rep("@(atom 1)"))

    def test_splice_unquote_reader_macro(self):
        self.assertEqual("(splice-unquote (1 2 3))", str(stepA_mal.READ("~@(1 2 3)")))

    def test_swap_assoc_get(self):
        self.assertEqual(
            '(atom {"+" #<function>})', self.rep('(def! e (atom {"+" +}))')
        )
        self.assertEqual(
            '{"+" #<function> "-" #<function>}', self.rep('(swap! e assoc "-" -)')
        )
        self.assertEqual("15", self.rep('( (get @e "+") 7 8)'))
        self.assertEqual("3", self.rep('( (get @e "-") 11 8)'))
        self.assertEqual(
            '{"+" #<function> "-" #<function> "foo" ()}',
            self.rep('(swap! e assoc "foo" (list))'),
        )
        self.assertEqual("()", self.rep('(get @e "foo")'))
        self.assertEqual(
            '{"+" #<function> "-" #<function> "foo" () "bar" (1 2 3)}',
            self.rep('(swap! e assoc "bar" \'(1 2 3))'),
        )
        self.assertEqual("(1 2 3)", self.rep('(get @e "bar")'))


if __name__ == "__main__":
    unittest.main()
