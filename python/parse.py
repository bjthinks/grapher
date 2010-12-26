from tokenize import *
from function import *
import unittest


class ParseError(Exception):
    pass


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
            raise ParseError()

    def expression(self, precedence):
        # precedence tells when to stop.
        # 0 => never stop
        # 1 => stop on +
        # 2 => stop on *
        result = self.atom()
        while self.peek().type == 'symbol':
            if precedence < 1 and self.peek().datum == '+':
                self.consume()
                result = Function.sum(result, self.expression(1))
            elif precedence < 2 and self.peek().datum == '*':
                self.consume()
                result = Function.product(result, self.expression(2))
            else:
                break
        return result

    def go(self):
        result = self.expression(0)
        if self.peek().type != 'eof':
            raise ParseError()
        return result

class _ParseUnitTests(unittest.TestCase):
    def matches(self, input_str, desired_function):
        self.assertEqual(str(Parse(tokenize(input_str)).go()),
                         str(desired_function))

    def errors(self, input_str):
        self.assertRaises(ParseError, Parse(tokenize(input_str)).go)

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
        self.matches('x+x', s(x, x))
        self.matches('x+x+x', s(s(x, x), x))
        self.matches('2+x+x', s(s(c(2), x), x))
        self.matches('x+2+x', s(s(x, c(2)), x))
        self.matches('x+x+2', s(s(x, x), c(2)))

        self.matches('17*x', p(c(17), x))
        self.matches('x*x', p(x, x))
        self.matches('x*x*x', p(p(x, x), x))
        self.matches('2*x*x', p(p(c(2), x), x))
        self.matches('x*2*x', p(p(x, c(2)), x))
        self.matches('x*x*2', p(p(x, x), c(2)))

    def test_precedence(self):
        x = Function.identity()
        def c(value):
            return Function.constant(float(value))
        s = Function.sum
        p = Function.product

        self.matches('2*x+3', s(p(c(2), x), c(3)))
        self.matches('2*x+x', s(p(c(2), x), x))
        self.matches('x*x+3', s(p(x, x), c(3)))
        self.matches('x*x+x', s(p(x, x), x))
        self.matches('3+2*x', s(c(3), p(c(2), x)))
        self.matches('x+2*x', s(x, p(c(2), x)))
        self.matches('3+x*x', s(c(3), p(x, x)))
        self.matches('x+x*x', s(x, p(x, x)))

    def test_errors(self):
        self.errors('1+')
        self.errors('+1')
        self.errors('++')
        self.errors('')
        self.errors('1*')
        self.errors('*1')
        self.errors('**')
        self.errors('1 1')

if __name__ == '__main__':
    unittest.main()
