from tokenize import *
from function import *
import unittest


ONE = Function.constant(1.0)
ONE_OVER_LOG_TEN = Function.constant(1.0/log(10.0))
KNOWN_FUNCTIONS = {
    'sin': Function.sin,
    'cos': Function.cos,
    'tan': lambda x: Function.quotient(Function.sin(x), Function.cos(x)),
    'cot': lambda x: Function.quotient(Function.cos(x), Function.sin(x)),
    'sec': lambda x: Function.quotient(ONE, Function.cos(x)),
    'csc': lambda x: Function.quotient(ONE, Function.sin(x)),
    'ln': Function.log,
    'log': lambda x: Function.product(ONE_OVER_LOG_TEN, Function.log(x)),
    }


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

    def atom(self, allow_unary_minus, allow_function):
        if self.peek().type == 'variable':
            if self.peek().datum != 'x':
                raise ParseError()
            self.consume()
            return Function.identity()
        elif self.peek().type == 'number':
            return Function.constant(self.consume().datum)
        elif self.peek().type == 'symbol':
            if self.peek().datum == '(':
                self.consume()
                result = self.expression(0)
                if self.peek().type != 'symbol' or self.peek().datum != ')':
                    raise ParseError()
                self.consume()
                return result
            elif self.peek().datum == '-' and allow_unary_minus:
                self.consume()
                # TODO: -x^2
                return Function.product(Function.constant(-1.0),
                                        self.expression(2, False))
        elif self.peek().type == 'function' and allow_function:
            function_name = self.peek().datum
            if function_name not in KNOWN_FUNCTIONS:
                raise ParseError()
            self.consume()
            return KNOWN_FUNCTIONS[function_name](self.expression(2, True, True))
        raise ParseError()

    def expression(self, precedence, allow_unary_minus = True,
                   inside_function = False, allow_function = True):
        # precedence tells when to stop.
        # 0 => never stop
        # 1 => stop on +
        # 2 => stop on *
        # 3 => stop on juxtaposition
        # 4 => stop on ^
        result = self.atom(allow_unary_minus, allow_function)
        while True:
            is_symbol = (self.peek().type == 'symbol')
            if is_symbol and precedence < 1 and self.peek().datum == '+':
                self.consume()
                result = Function.sum(result, self.expression(1))
            elif is_symbol and self.peek().datum == '-':
                if precedence >= 1:
                    break
                self.consume()
                result = Function.sum(
                    result, Function.product(
                        Function.constant(-1.0), self.expression(1)))
            elif is_symbol and precedence < 2 and self.peek().datum == '*':
                self.consume()
                result = Function.product(result, self.expression(2))
            elif is_symbol and precedence < 2 and self.peek().datum == '/':
                self.consume()
                result = Function.quotient(result, self.expression(2))
            elif is_symbol and precedence < 4 and self.peek().datum == '^':
                self.consume()
                result = Function.power(result, self.expression(3))
            elif precedence < 3:
                # Paul doesn't like this
                old_pos = self.pos
                try:
                    result = Function.product(result, self.expression(3, inside_function = inside_function, allow_function = not inside_function))
                except ParseError:
                    self.pos = old_pos
                    break
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
        x = Function.identity()
        def c(value):
            return Function.constant(float(value))
        self.matches('x', x)
        self.matches('1', Function.constant(1.0))
        self.matches('4', Function.constant(4.0))
        self.matches('sin x', Function.sin(x))
        self.matches('cos x', Function.cos(x))
        self.matches('tan x', Function.quotient(Function.sin(x),
                                                Function.cos(x)))
        self.matches('cot x', Function.quotient(Function.cos(x),
                                                Function.sin(x)))
        self.matches('sec x', Function.quotient(c(1), Function.cos(x)))
        self.matches('csc x', Function.quotient(c(1), Function.sin(x)))
        self.matches('ln x', Function.log(x))
        self.matches('log x', Function.product(c(1.0/log(10.0)),
                                               Function.log(x)))

    def test_basic_operators(self):
        x = Function.identity()
        def c(value):
            return Function.constant(float(value))
        s = Function.sum
        p = Function.product
        q = Function.quotient
        def d(arg1, arg2):
            return s(arg1, p(c(-1), arg2))
        e = Function.power

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

        self.matches('(x)', x)
        self.matches('(1)', c(1))

        self.matches('x/2', q(x, c(2)))

        self.matches('x-2', d(x, c(2)))

        self.matches('-x', p(c(-1), x))
        self.matches('-2', p(c(-1), c(2)))
        self.matches('-(-2)', p(c(-1), p(c(-1), c(2))))

        self.matches('x^x', e(x, x))

        self.matches('x x', p(x, x))
        self.matches('1 1', p(c(1), c(1)))

    def test_precedence(self):
        x = Function.identity()
        def c(value):
            return Function.constant(float(value))
        s = Function.sum
        p = Function.product
        q = Function.quotient
        def d(arg1, arg2):
            return s(arg1, p(c(-1), arg2))
        e = Function.power
        sin = Function.sin
        cos = Function.cos

        self.matches('2*x+3', s(p(c(2), x), c(3)))
        self.matches('2*x+x', s(p(c(2), x), x))
        self.matches('x*x+3', s(p(x, x), c(3)))
        self.matches('x*x+x', s(p(x, x), x))
        self.matches('3+2*x', s(c(3), p(c(2), x)))
        self.matches('x+2*x', s(x, p(c(2), x)))
        self.matches('3+x*x', s(c(3), p(x, x)))
        self.matches('x+x*x', s(x, p(x, x)))
        self.matches('x*x*x+x*x+x+3+x*x*x*x',
                     s(s(s(s(p(p(x,x),x), p(x,x)), x), c(3)),
                       p(p(p(x,x),x),x)))
        self.matches('2*(x+3)', p(c(2), s(x, c(3))))
        self.matches('2*(x+x)', p(c(2), s(x, x)))
        self.matches('x*(x+3)', p(x, s(x, c(3))))
        self.matches('x*(x+x)', p(x, s(x, x)))
        self.matches('(3+2)*x', p(s(c(3), c(2)), x))
        self.matches('(x+2)*x', p(s(x, c(2)), x))
        self.matches('(3+x)*x', p(s(c(3), x), x))
        self.matches('(x+x)*x', p(s(x, x), x))
        self.matches('x*x/2', q(p(x, x), c(2)))
        self.matches('x+x/2', s(x, q(x, c(2))))
        self.matches('x/2*x', p(q(x, c(2)), x))
        self.matches('x/(2*x)', q(x, p(c(2), x)))
        self.matches('x/2/2', q(q(x, c(2)), c(2)))
        self.matches('x+x-2', d(s(x, x), c(2)))
        self.matches('x-x+2', s(d(x, x), c(2)))
        self.matches('x-x-2', d(d(x, x), c(2)))
        self.matches('x*x-2', d(p(x, x), c(2)))
        self.matches('x-x*2', d(x, p(x, c(2))))
        self.matches('x--2', d(x, c(-2)))
        self.matches('-x-2', d(p(c(-1), x), c(2)))
        self.matches('-x--2', d(p(c(-1), x), c(-2)))
        self.matches('-x*x/-3-x/5+-4', s(d(q(p(p(c(-1), x), x), c(-3)),
                                           q(x, c(5))), c(-4)))
        self.matches('x^x^x', e(x, e(x, x)))
        self.matches('x*x^2', p(x, e(x, c(2))))
        self.matches('x^x*2', p(e(x, x), c(2)))
        self.matches('x^-2', e(x, c(-2)))
        self.matches('-x^2', p(c(-1), e(x, c(2))))
        self.matches('-x^-2', p(c(-1), e(x, c(-2))))
        self.matches('x (x)', p(x, x))
        self.matches('(x) x', p(x, x))
        self.matches('x x+x', s(p(x, x), x))
        self.matches('x+x x', s(x, p(x, x)))
        self.matches('x x-x', d(p(x, x), x))
        self.matches('x-x x', d(x, p(x, x)))
        self.matches('x x*x', p(p(x, x), x))
        self.matches('x*x x', p(x, p(x, x)))
        self.matches('x x/x', q(p(x, x), x))
        self.matches('x/x x', q(x, p(x, x)))
        self.matches('2 x/x', q(p(c(2), x), x))
        self.matches('x/2 x', q(x, p(c(2), x)))
        self.matches('x^x x', p(e(x, x), x))
        self.matches('x^2 x', p(e(x, c(2)), x))
        self.matches('x x^x', p(x, e(x, x)))
        self.matches('x^2 x^3', p(e(x, c(2)), e(x,c(3))))
        self.matches('(2)-1', d(c(2), c(1)))
        self.matches('(2)-(1)', d(c(2), c(1)))
        self.matches('2-(1)', d(c(2), c(1)))
        self.matches('2(-1)', p(c(2), c(-1)))
        self.matches('x^x*x', p(e(x, x), x))
        self.matches('x*x^x', p(x, e(x, x)))
        self.matches('x^x/x', q(e(x, x), x))
        self.matches('x/x^x', q(x, e(x, x)))
        self.matches('sin x^x', sin(e(x, x)))
        self.matches('sin -x', sin(p(c(-1), x)))
        self.matches('sin 2x', sin(p(c(2), x)))
        self.matches('sin x*x', p(sin(x), x))
        self.matches('sin x/x', q(sin(x), x))
        self.matches('sin x+x', s(sin(x), x))
        self.matches('sin x-x', d(sin(x), x))
        self.matches('sin sin x', sin(sin(x)))
        self.matches('sin x sin x', p(sin(x), sin(x)))
        self.matches('sin x x^x cos x', p(sin(p(x, e(x, x))), cos(x)))
        self.matches('sin x^x cos x', p(sin(e(x, x)), cos(x)))
        self.matches('sin cos x x^x', sin(cos(p(x, e(x, x)))))
        self.matches('sin cos x^x x', sin(cos(p(e(x, x), x))))
        self.matches('sin x cos x^x', p(sin(x), cos(e(x, x))))
        self.matches('sin x^cos x', sin(e(x, cos(x))))
        self.matches('sin x^cos x^x', sin(e(x, cos(e(x, x)))))
        self.matches('sin x^x^cos x', sin(e(x, e(x, cos(x)))))

    def test_errors(self):
        self.errors('y')
        self.errors('xx')
        self.errors('1+')
        self.errors('+1')
        self.errors('++')
        self.errors('')
        self.errors('1*')
        self.errors('*1')
        self.errors('**')
        self.errors('--1')
        self.errors('(2-)1')
        self.errors('(2(-)1)')
        self.errors('sin')

if __name__ == '__main__':
    unittest.main()
