from tokenize import *
from function import *
import unittest


class Parse(object):
    def __init__(self, tokens):
        pass

    def go(self):
        return Function.identity()


class _ParseUnitTests(unittest.TestCase):
    def matches(self, input_str, desired_function):
        self.assertEqual(str(Parse(tokenize(input_str)).go()),
                         str(desired_function))

    def test_basic_functions(self):
        self.matches('x', Function.identity())


if __name__ == '__main__':
    unittest.main()
