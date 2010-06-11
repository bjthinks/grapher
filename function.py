from __future__ import division
import abc
from interval import Interval
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
    def sum(f, g):
        if not (isinstance(f, Function) and isinstance(g, Function)):
            raise ValueError
        if isinstance(f, _ConstantFunction) and f._k == 0:
            return g
        if isinstance(g, _ConstantFunction) and g._k == 0:
            return f
        if isinstance(f, _ConstantFunction) and \
                isinstance(g, _ConstantFunction):
            return _ConstantFunction(f._k + g._k)
        return _SumFunction(f, g)

    @staticmethod
    def product(f, g):
        if not (isinstance(f, Function) and isinstance(g, Function)):
            raise ValueError
        if isinstance(f, _ConstantFunction) and f._k == 0:
            return f
        if isinstance(g, _ConstantFunction) and g._k == 0:
            return g
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
        if isinstance(g, _ConstantFunction) and g._k == 0:
            return Function.constant(1)
        if isinstance(f, _ConstantFunction) and f._k == 1:
            return Function.constant(1)
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

    def __str__(self):
        return str(self._k)

    def __repr__(self):
        return 'Function.constant({0!r})'.format(self._k)


class _IdentityFunction(Function):
    def __call__(self, param):
        return param

    def derivative(self):
        return self.constant(1)

    def __str__(self):
        return 'x'

    def __repr__(self):
        return 'Function.identity()'


class _SumFunction(Function):
    def __init__(self, f, g):
        assert isinstance(f, Function)
        assert isinstance(g, Function)
        self.__f = f
        self.__g = g

    def __call__(self, param):
        return self.__f(param) + self.__g(param)

    def derivative(self):
        return Function.sum(self.__f.derivative(), self.__g.derivative())

    def __str__(self):
        return '({0} + {1})'.format(self.__f, self.__g)

    def __repr__(self):
        return 'Function.sum({0!r}, {1!r})'.format(self.__f, self.__g)


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
                         '(x + 2)')
        self.assertEqual(repr(Function.sum(Function.identity(),
                                           Function.constant(2))),
                         'Function.sum(Function.identity(), ' +
                         'Function.constant(2))')
        self.assertEqual(Function.sum(Function.identity(),
                                      Function.constant(5)).derivative()(8), 1)
        self.numericalDerivativeTest(Function.sum(Function.identity(),
                                                  Function.identity()))
        self.numericalDerivativeTest(Function.sum(Function.identity(),
                                                  Function.constant(5)))

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
        # x*0 = 0
        self.assertEqual(str(Function.product(x, c(0))), '0')
        # 0*x = 0
        self.assertEqual(str(Function.product(c(0), x)), '0')
        # x*1 = x
        self.assertEqual(str(Function.product(x, c(1))), 'x')
        # 1*x = x
        self.assertEqual(str(Function.product(c(1), x)), 'x')
        # const*const = const
        self.assertEqual(str(Function.product(c(2), c(3))), '6')

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
        pow = Function.power
        f = Function.quotient(plus(pow(x, c(2)), c(1)),
                              plus(pow(x, c(3)), c(1)))
        self.assertEqual(
            str(f.derivative()),
            '((((x * 2) * ((x ** 3) + 1))'
            ' + (-1 * (((x ** 2) + 1) * ((x ** 2) * 3))))'
            ' / (((x ** 3) + 1) ** 2))')

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
        # const^const = const
        self.assertEqual(str(Function.power(c(2), c(3))), '8')
        # x^0 = 1
        self.assertEqual(str(Function.power(x, c(0))), '1')
        # 1^x = 1
        self.assertEqual(str(Function.power(c(1), x)), '1')
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


if __name__ == '__main__':
    unittest.main()
