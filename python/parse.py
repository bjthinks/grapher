from tokenize import *
from function import *
import unittest


class Parse(object):
    def __init__(self, tokens):
        pass

    def go(self):
        return Function.identity()


class _ParseUnitTests(unittest.TestCase):
    def test_basic_functions(self):
        self.assertEqual(str(Parse([Token('variable', 'x')]).go()),
                         str(Function.identity()))


if __name__ == '__main__':
    unittest.main()
