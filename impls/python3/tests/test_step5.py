import unittest

import step5_tco


class TestStep5(unittest.TestCase):
    def test_step5_tco(self):
        self.assertEqual(
            "#<function>",
            step5_tco.rep(
                "(def! sum2 (fn* (n acc) (if (= n 0) acc (sum2 (- n 1) (+ n acc)))))"
            ),
        )
        self.assertEqual("55", step5_tco.rep("(sum2 10 0)"))
        self.assertEqual("nil", step5_tco.rep("(def! res2 nil)"))
        self.assertEqual("500500", step5_tco.rep("(def! res2 (sum2 1000 0))"))
        self.assertEqual("500500", step5_tco.rep("res2"))


if __name__ == "__main__":
    unittest.main()
