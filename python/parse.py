from tokenize import *
from function import *
import unittest


class Parse(object):
    def __init__(self, tokens):
        self.tokens = list(tokens)
        self.pos = 0

    def peek(self):
        if self.pos < len(self.tokens):
            return self.tokens[self.pos]
        else:
            return Token('eof', None)

    def consume(self):
        # Note: shouldn't call this if at EOF.
        result = self.peek()
        self.pos += 1
        return result

    def atom(self):
        if self.peek().type == 'variable':
            # It must be an x, who uses other letters anyway?
            self.consume()
            return Function.identity()
        elif self.peek().type == 'number':
            return Function.constant(self.consume().datum)
        else:
            raise "hell"

    def go(self):
        result = self.atom()
        while self.peek().type == 'symbol':
            if self.peek().datum == '+':
                self.consume()
                result = Function.sum(result, self.atom())
            elif self.peek().datum == '*':
                self.consume()
                result = Function.product(result, self.atom())
        return result

class _ParseUnitTests(unittest.TestCase):
    def matches(self, input_str, desired_function):
        self.assertEqual(str(Parse(tokenize(input_str)).go()),
                         str(desired_function))

    def test_basic_functions(self):
        self.matches('x', Function.identity())
        self.matches('1', Function.constant(1.0))
        self.matches('4', Function.constant(4.0))

    def test_basic_operators(self):
        x = Function.identity()
        def c(value):
            return Function.constant(float(value))
        s = Function.sum
        p = Function.product

        self.matches('x+17', s(x, c(17)))
        self.matches('17*x', p(c(17), x))
        self.matches('x*x', p(x, x))
        self.matches('x*x', p(x, x))
        self.matches('x*x*x', p(p(x, x), x))


if __name__ == '__main__':
    unittest.main()
