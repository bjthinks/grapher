from tokenize import *
from function import *
import unittest


class Parse(object):
    def __init__(self, tokens):
        self.tokens = list(tokens)

    def go(self):
        if self.tokens[0].type == 'variable':
            # It must be an x, who uses other letters anyway?
            return Function.identity()
        elif self.tokens[0].type == 'number':
            return Function.constant(self.tokens[0].datum)
        else:
            raise "hell"

class _ParseUnitTests(unittest.TestCase):
    def matches(self, input_str, desired_function):
        self.assertEqual(str(Parse(tokenize(input_str)).go()),
                         str(desired_function))

    def test_basic_functions(self):
        self.matches('x', Function.identity())
        self.matches('1', Function.constant(1.0))
        self.matches('4', Function.constant(4.0))


if __name__ == '__main__':
    unittest.main()
