from __future__ import division
import abc
from interval import Interval
import math
from math import log, floor
import unittest


class Function(object):
    __metaclass__ = abc.ABCMeta

    @abc.abstractmethod
    def __call__(self, param):
        pass

    @abc.abstractmethod
    def derivative(self):
        pass

    @abc.abstractmethod
    def weak_simplify(self):
        """Perform "weak simplifications", meaning simplifications
        that may possibly expand the domain of the function."""
        pass

    @abc.abstractmethod
    def __str__(self):
        pass

    @abc.abstractmethod
    def __repr__(self):
        pass

    @staticmethod
    def constant(k):
        return _ConstantFunction(k)

    @staticmethod
    def identity():
        return _IdentityFunction()

    @staticmethod
    def sum(*args):
        if not all(isinstance(f, Function) for f in args):
            raise ValueError
        # Expand any args that are _SumFunctions into their terms.
        expanded_args = []
        for f in args:
            if isinstance(f, _SumFunction):
                expanded_args.extend(f._terms)
            else:
                expanded_args.append(f)
        # Collect constants
        const_term = 0
        other_terms = []
        for f in expanded_args:
            if isinstance(f, _ConstantFunction):
                const_term += f._k
            else:
                other_terms.append(f)
        # Do other simplifications
        if len(other_terms) == 0:
            return _ConstantFunction(const_term)
        if const_term != 0:
            other_terms.insert(0, _ConstantFunction(const_term))
        if len(other_terms) == 1:
            return other_terms[0]
        return _SumFunction(tuple(other_terms))

    @staticmethod
    def product(f, g):
        if not (isinstance(f, Function) and isinstance(g, Function)):
            raise ValueError
        if isinstance(f, _ConstantFunction) and f._k == 1:
            return g
        if isinstance(g, _ConstantFunction) and g._k == 1:
            return f
        if isinstance(f, _ConstantFunction) and \
                isinstance(g, _ConstantFunction):
            return _ConstantFunction(f._k * g._k)
        return _ProductFunction(f, g)

    @staticmethod
    def quotient(f, g):
        if not (isinstance(f, Function) and isinstance(g, Function)):
            raise ValueError
        return _QuotientFunction(f, g)

    @staticmethod
    def power(f, g):
        if not (isinstance(f, Function) and isinstance(g, Function)):
            raise ValueError
        if isinstance(f, _ConstantFunction) and \
                isinstance(g, _ConstantFunction):
            return Function.constant(f._k ** g._k)
        if isinstance(g, _ConstantFunction) and g._k == 1:
            return f
        # This is very specific, but we think it will occur lots
        if isinstance(g, _ConstantFunction) and g._k == floor(g._k) and \
                isinstance(f, _PowerFunction) and \
                isinstance(f._g, _ConstantFunction) and \
                f._g._k == floor(f._g._k) and \
                not (g._k < 0 and f._g._k < 0):
            return Function.power(f._f, Function.constant(f._g._k * g._k))
        return _PowerFunction(f, g)

    @staticmethod
    def log(f):
        if not isinstance(f, Function):
            raise ValueError
        return _LogFunction(f)


class _ConstantFunction(Function):
    def __init__(self, k):
        # A ConstantFunction can't be initialized using an interval--it
        # is intended to represent a single constant value.
        assert not isinstance(k, Interval)
        self._k = k

    def __call__(self, param):
        return self._k

    def derivative(self):
        return self.constant(0)

    def weak_simplify(self):
        return self

    def __str__(self):
        return str(self._k)

    def __repr__(self):
        return 'Function.constant({0!r})'.format(self._k)


class _IdentityFunction(Function):
    def __call__(self, param):
        return param

    def derivative(self):
        return self.constant(1)

    def weak_simplify(self):
        return self

    def __str__(self):
        return 'x'

    def __repr__(self):
        return 'Function.identity()'


class _SumFunction(Function):
    def __init__(self, terms):
        assert isinstance(terms, tuple)
        assert len(terms) >= 2
        assert all(isinstance(f, Function) for f in terms)
        self._terms = terms

    def __call__(self, param):
        return sum(f(param) for f in self._terms)

    def derivative(self):
        return Function.sum(*[f.derivative() for f in self._terms])

    def weak_simplify(self):
        return Function.sum(*[f.weak_simplify() for f in self._terms])

    def __str__(self):
        return '({0})'.format(' + '.join(str(f) for f in self._terms))

    def __repr__(self):
        return 'Function.sum({0})'.format(
            ', '.join(repr(f) for f in self._terms))


class _ProductFunction(Function):
    def __init__(self, f, g):
        assert isinstance(f, Function)
        assert isinstance(g, Function)
        self.__f = f
        self.__g = g

    def __call__(self, param):
        return self.__f(param) * self.__g(param)

    def derivative(self):
        return Function.sum(Function.product(self.__f.derivative(), self.__g),
                            Function.product(self.__f, self.__g.derivative()))

    def weak_simplify(self):
        f = self.__f.weak_simplify()
        g = self.__g.weak_simplify()
        if isinstance(f, _ConstantFunction) and f._k == 0:
            return f
        if isinstance(g, _ConstantFunction) and g._k == 0:
            return g
        return Function.product(f, g)

    def __str__(self):
        return '({0} * {1})'.format(self.__f, self.__g)

    def __repr__(self):
        return 'Function.product({0!r}, {1!r})'.format(self.__f, self.__g)


class _QuotientFunction(Function):
    def __init__(self, f, g):
        assert isinstance(f, Function)
        assert isinstance(g, Function)
        self.__f = f
        self.__g = g

    def __call__(self, param):
        # Do we want to convert ZeroDivisionError into ValueError here?
        return self.__f(param) / self.__g(param)

    def derivative(self):
        return Function.quotient(Function.sum(
                Function.product(self.__f.derivative(), self.__g),
                Function.product(Function.constant(-1),
                                 Function.product(self.__f,
                                                  self.__g.derivative()))),
                                 Function.power(self.__g,
                                                Function.constant(2)))

    def weak_simplify(self):
        f = self.__f.weak_simplify()
        g = self.__g.weak_simplify()
        return Function.quotient(f, g)

    def __str__(self):
        return '({0} / {1})'.format(self.__f, self.__g)

    def __repr__(self):
        return 'Function.quotient({0!r}, {1!r})'.format(self.__f, self.__g)


class _PowerFunction(Function):
    def __init__(self, f, g):
        assert isinstance(f, Function)
        assert isinstance(g, Function)
        self._f = f
        self._g = g

    def __call__(self, param):
        return self._f(param) ** self._g(param)

    def derivative(self):
        # f(x) ** g(x) * Dg(x) * log(f(x)) + f(x) ** (g(x) - 1) * g(x) * Df(x)
        return Function.sum(
            Function.product(self,
                             Function.product(self._g.derivative(),
                                              Function.log(self._f))),
            Function.product(Function.power(
                    self._f, Function.sum(self._g, Function.constant(-1))),
                             Function.product(self._g, self._f.derivative())))
    
    def weak_simplify(self):
        f = self._f.weak_simplify()
        g = self._g.weak_simplify()
        if isinstance(g, _ConstantFunction) and g._k == 0:
            return Function.constant(1)
        if isinstance(f, _ConstantFunction) and f._k == 1:
            return Function.constant(1)
        return Function.power(f, g)

    def __str__(self):
        return '({0} ** {1})'.format(self._f, self._g)

    def __repr__(self):
        return 'Function.power({0!r}, {1!r})'.format(self._f, self._g)


class _LogFunction(Function):
    def __init__(self, f):
        assert isinstance(f, Function)
        self.__f = f

    def __call__(self, param):
        f_value = self.__f(param)
        if isinstance(f_value, Interval):
            return Interval.log(f_value)
        else:
            return log(f_value)

    def derivative(self):
        # d ln f(x)/dx = f'(x)/f(x)
        return Function.quotient(self.__f.derivative(), self.__f)

    def weak_simplify(self):
        f = self.__f.weak_simplify()
        return Function.log(f)

    def __str__(self):
        return 'ln({0})'.format(self.__f)

    def __repr__(self):
        return 'Function.log({0})'.format(repr(self.__f))


class _FunctionUnitTests(unittest.TestCase):
    def intervals(self):
        for p in xrange(-2, 3):
            for q in xrange(-2, 3):
                yield Interval(p, q)

    @staticmethod
    def interval_function(i):
        """Create a function which, when evaluated over the interval
        [0, 1], takes on values in the interval i."""
        assert isinstance(i, Interval)
        return Function.sum(
            Function.product(Function.constant(i.right - i.left),
                             Function.identity()),
            Function.constant(i.left))

    def numericalDerivativeTest(self, f):
        h = 1e-6
        tol = 1e-4
        points = [ 2.74557456, 1.1436346, 1.743563456 ]
        deriv = f.derivative()
        for x in points:
            v1 = deriv(x)
            v2 = (f(x+h/2) - f(x-h/2)) / h
            # We want v1 and v2 to be within tol of each other
            self.assertTrue(abs(v1-v2) / abs(v1+v2) < tol)

    def test_const(self):
        for val in xrange(5):
            self.assertEqual(val, Function.constant(val)(87))
            self.assertEqual(val, Function.constant(val)(Interval(2,6)))
            self.assertEqual(str(Function.constant(val).weak_simplify()), str(val))
        self.assertEqual(str(Function.constant(3)), '3')
        self.assertEqual(repr(Function.constant(3)), 'Function.constant(3)')
        self.assertEqual(Function.constant(5).derivative()(3.67), 0)

    def test_identity(self):
        for val in xrange(5):
            self.assertEqual(val, Function.identity()(val))
        for i in self.intervals():
            self.assertEqual(i, Function.identity()(i))
        self.assertEqual(str(Function.identity()), 'x')
        self.assertEqual(repr(Function.identity()), 'Function.identity()')
        self.assertEqual(Function.identity().derivative()(6), 1)
        self.numericalDerivativeTest(Function.identity())
        self.assertEqual(str(Function.identity().weak_simplify()), 'x')

    def test_sum(self):
        for val in xrange(5):
            self.assertEqual(val+34, Function.sum(Function.constant(34),
                                                  Function.identity())(val))
        for i in self.intervals():
            for j in self.intervals():
                sum_func = Function.sum(self.interval_function(i),
                                        self.interval_function(j))
                self.assertEqual(i+j, sum_func(Interval(0, 1)))
        self.assertEqual(str(Function.sum(Function.identity(),
                                          Function.constant(2))),
                         '(2 + x)')
        self.assertEqual(repr(Function.sum(Function.identity(),
                                           Function.constant(2))),
                         'Function.sum(Function.constant(2), ' +
                         'Function.identity())')
        self.assertEqual(Function.sum(Function.identity(),
                                      Function.constant(5)).derivative()(8), 1)
        self.numericalDerivativeTest(Function.sum(Function.identity(),
                                                  Function.identity()))
        self.numericalDerivativeTest(Function.sum(Function.identity(),
                                                  Function.constant(5)))
        c = Function.constant
        x = Function.identity()
        prod = Function.product
        log = Function.log
        # Variable argument count
        self.assertEqual(str(Function.sum()), '0')
        self.assertEqual(str(Function.sum(c(3))), '3')
        self.assertEqual(str(Function.sum(x)), 'x')
        f = Function.sum(c(3), x, log(x))
        self.assertEqual(str(f), '(3 + x + ln(x))')
        self.assertEqual(repr(f),
                         'Function.sum(Function.constant(3), '
                         'Function.identity(), '
                         'Function.log(Function.identity()))')
        self.assertEqual(f(2), 3 + 2 + math.log(2))
        self.numericalDerivativeTest(f)
        # Simplifications
        # 0*x + 2 = 2
        self.assertEqual(
            str(Function.sum(prod(c(0), x), c(2)).weak_simplify()), '2')
        # 2 + 0*x = 2
        self.assertEqual(
            str(Function.sum(c(2), prod(c(0), x)).weak_simplify()), '2')
        # 2 + 3 = 5
        self.assertEqual(str(Function.sum(c(2), c(3))), '5')
        # (x + 1) + 1 = 2 + x
        self.assertEqual(str(Function.sum(Function.sum(x, c(1)), c(1))),
                         '(2 + x)')

    def test_product(self):
        for val in xrange(5):
            self.assertEqual(val*34, Function.product(
                    Function.constant(34), Function.identity())(val))
        for i in self.intervals():
            for j in self.intervals():
                self.assertEqual(i*j,
                                 Function.product(self.interval_function(i),
                                                  self.interval_function(j))
                                 (Interval(0, 1)))
        self.assertEqual(str(Function.product(Function.identity(),
                                              Function.constant(2))),
                         '(x * 2)')
        self.assertEqual(repr(Function.product(Function.identity(),
                                               Function.constant(2))),
                         'Function.product(Function.identity(), ' +
                         'Function.constant(2))')
        self.assertEqual(Function.product(
                Function.identity(), Function.constant(4)).derivative()(7), 4)
        self.numericalDerivativeTest(Function.product(Function.identity(),
                                                      Function.identity()))
        self.numericalDerivativeTest(Function.product(Function.identity(),
                                                      Function.constant(5)))
        # Simplifications:
        c = Function.constant
        x = Function.identity()
        prod = Function.product
        pow = Function.power
        # x*0 = 0
        self.assertEqual(str(prod(x, c(0)).weak_simplify()), '0')
        # 0*x = 0
        self.assertEqual(str(prod(c(0), x).weak_simplify()), '0')
        # x*1 = x
        self.assertEqual(str(prod(x, c(1))), 'x')
        # 1*x = x
        self.assertEqual(str(prod(c(1), x)), 'x')
        # const*const = const
        self.assertEqual(str(prod(c(2), c(3))), '6')
        # (x*0)*x = 0
        self.assertEqual(str(prod(prod(x, c(0)), x).weak_simplify()), '0')
        # x*(x*0) = 0
        self.assertEqual(str(prod(x, prod(x, c(0))).weak_simplify()), '0')
        # (x^0)*x = x
        self.assertEqual(str(prod(pow(x, c(0)), x).weak_simplify()), 'x')
        # x*(x^0) = x
        self.assertEqual(str(prod(x, pow(x, c(0))).weak_simplify()), 'x')

    def test_quotient(self):
        for v in xrange(5):
            for w in xrange(1,5):
                self.assertEqual(v/w, Function.quotient(
                        Function.constant(v), Function.identity())(w))
        self.assertEqual(str(Function.quotient(Function.constant(1),
                                               Function.constant(2))),
                         '(1 / 2)')
        self.assertEqual(repr(Function.quotient(Function.constant(1),
                                                Function.constant(2))),
                         'Function.quotient(Function.constant(1), ' +
                         'Function.constant(2))')
        # Floating point is exact for small powers of two
        self.assertEqual(Function.quotient(
                Function.constant(1), Function.identity()).derivative()(2),
                         -0.25)
        self.numericalDerivativeTest(Function.quotient(Function.constant(3),
                                                       Function.identity()))
        self.numericalDerivativeTest(Function.quotient(Function.identity(),
                                                       Function.constant(5)))
        # Test the derivative formula on (x^2+1)/(x^3+1)
        x = Function.identity()
        c = Function.constant
        plus = Function.sum
        prod = Function.product
        pow = Function.power
        f = Function.quotient(plus(pow(x, c(2)), c(1)),
                              plus(pow(x, c(3)), c(1)))
        self.assertEqual(
            str(f.derivative().weak_simplify()),
            '((((x * 2) * (1 + (x ** 3)))'
            ' + (-1 * ((1 + (x ** 2)) * ((x ** 2) * 3))))'
            ' / ((1 + (x ** 3)) ** 2))')
        # Simplifications
        # (1 + x*0)/(2*x) = 1/(2*x)
        f = Function.quotient(plus(c(1), prod(x, c(0))), prod(c(2), x))
        self.assertEqual(str(f.weak_simplify()), '(1 / (2 * x))')
        # 1/(x + x*0) = 1/x
        f = Function.quotient(c(1), plus(x, prod(x, c(0))))
        self.assertEqual(str(f.weak_simplify()), '(1 / x)')

    def test_power(self):
        for val in xrange(5):
            self.assertEqual(34**val, Function.power(Function.constant(34),
                                                     Function.identity())(val))
        for val in xrange(5):
            self.assertEqual(val**34, Function.power(
                    Function.identity(), Function.constant(34))(val))
        for i in self.intervals():
            try:
                expected = i**i
            except ValueError:
                continue
            self.assertEqual(expected, Function.power(Function.identity(),
                                                      Function.identity())(i))
        self.assertEqual(str(Function.power(Function.identity(),
                                            Function.constant(2))),
                         '(x ** 2)')
        self.assertEqual(repr(Function.power(Function.identity(),
                                             Function.constant(2))),
                         'Function.power(Function.identity(), ' +
                         'Function.constant(2))')
        for v in [2, 3, 4, 5, 6.666]:
            self.numericalDerivativeTest(Function.power(
                    Function.identity(), Function.constant(v)))
            self.numericalDerivativeTest(Function.power(
                    Function.constant(v), Function.identity()))
        self.numericalDerivativeTest(Function.power(
                Function.identity(), Function.identity()))
        # x^2 + 3x + 5
        f = Function.power(Function.identity(), Function.constant(2))
        f = Function.sum(f, Function.product(Function.identity(),
                                             Function.constant(3)))
        f = Function.sum(f, Function.constant(5))
        # 4x^2 + 7x + 2
        g = Function.power(Function.identity(), Function.constant(2))
        g = Function.product(g, Function.constant(4))
        g = Function.sum(g, Function.product(Function.identity(),
                                             Function.constant(7)))
        g = Function.sum(g, Function.constant(2))
        self.numericalDerivativeTest(Function.power(f, g))
        # Simplifications:
        c = Function.constant
        x = Function.identity()
        prod = Function.product
        sum = Function.sum
        # const^const = const
        self.assertEqual(str(Function.power(c(2), c(3))), '8')
        # x^0 = 1
        self.assertEqual(str(Function.power(x, c(0)).weak_simplify()), '1')
        # 1^x = 1
        self.assertEqual(str(Function.power(c(1), x).weak_simplify()), '1')
        # x^1 = x
        self.assertEqual(str(Function.power(x, c(1))), 'x')
        # (x^2)^3 = x^6
        self.assertEqual(str(Function.power(Function.power(x, c(2)), c(3))),
                         '(x ** 6)')
        # (x^-2)^3 = x^-6
        self.assertEqual(str(Function.power(Function.power(x, c(-2)), c(3))),
                         '(x ** -6)')
        # (x^2)^-3 = x^-6
        self.assertEqual(str(Function.power(Function.power(x, c(2)), c(-3))),
                         '(x ** -6)')
        # (x^-2)^-3 should not simplify because this would change the domain.
        self.assertEqual(str(Function.power(Function.power(x, c(-2)), c(-3))),
                         '((x ** -2) ** -3)')
        # 2^(3 + 0*x) = 8
        f = Function.power(c(2), sum(c(3), prod(c(0), x)))
        self.assertEqual(str(f.weak_simplify()), '8')
        # (2 + 0*x)^3 = 8
        f = Function.power(sum(c(2), prod(c(0), x)), c(3))
        self.assertEqual(str(f.weak_simplify()), '8')

    def test_natural_log(self):
        for val in xrange(1, 5):
            self.assertEqual(log(val),
                             Function.log(Function.identity())(val))
        for i in self.intervals():
            try:
                expected = i.log()
            except ValueError:
                continue
            self.assertEqual(expected,
                             Function.log(Function.identity())(i))
        self.assertEqual(str(Function.log(Function.constant(5))),
                         'ln(5)')
        self.assertEqual(repr(Function.log(Function.constant(5))),
                         'Function.log(Function.constant(5))')
        self.numericalDerivativeTest(Function.log(Function.identity()))
        self.numericalDerivativeTest(
            Function.log(
                Function.sum(Function.identity(), Function.constant(1))))
        self.numericalDerivativeTest(
            Function.log(
                Function.product(Function.identity(), Function.constant(2))))
        # Simplifications
        c = Function.constant
        x = Function.identity()
        prod = Function.product
        sum = Function.sum
        # ln (x + x*0) = ln x
        f = Function.log(sum(x, prod(x, c(0))))
        self.assertEqual(str(f.weak_simplify()), 'ln(x)')

if __name__ == '__main__':
    unittest.main()
