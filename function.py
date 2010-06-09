from __future__ import division
import abc
from interval import Interval
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
        return _ConstFunction(k)

    @staticmethod
    def identity():
        return _IdentityFunction()

    def __add__(self, g):
        if not isinstance(g, Function):
            return NotImplemented
        return _SumFunction(self, g)

    def __mul__(self, g):
        if not isinstance(g, Function):
            return NotImplemented
        return _ProductFunction(self, g)

    def __truediv__(self, g):
        if not isinstance(g, Function):
            return NotImplemented
        return _QuotientFunction(self, g)

    def __pow__(self, g):
        if not isinstance(g, Function):
            return NotImplemented
        return _PowerFunction(self, g)


class _ConstFunction(Function):
    def __init__(self, k):
        # A ConstFunction can't be initialized using an interval--it
        # is intended to represent a single constant value.
        assert not isinstance(k, Interval)
        self.__k = k

    def __call__(self, param):
        return self.__k

    def derivative(self):
        return self.constant(0)

    def __str__(self):
        return str(self.__k)

    def __repr__(self):
        return 'Function.constant({0})'.format(repr(self.__k))


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
        return self.__f.derivative() + self.__g.derivative()

    def __str__(self):
        return '({0} + {1})'.format(str(self.__f), str(self.__g))

    def __repr__(self):
        return '({0} + {1})'.format(repr(self.__f), repr(self.__g))


class _ProductFunction(Function):
    def __init__(self, f, g):
        assert isinstance(f, Function)
        assert isinstance(g, Function)
        self.__f = f
        self.__g = g

    def __call__(self, param):
        return self.__f(param) * self.__g(param)

    def derivative(self):
        return self.__f.derivative() * self.__g + \
            self.__f * self.__g.derivative()

    def __str__(self):
        return '({0} * {1})'.format(str(self.__f), str(self.__g))

    def __repr__(self):
        return '({0} * {1})'.format(
            repr(self.__f), repr(self.__g))


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
        return ((self.__f.derivative() * self.__g +
                 self.constant(-1) * self.__f * self.__g.derivative())
                # this part should use _PowerFunction
                / (self.__g * self.__g))

    def __str__(self):
        return '({0} / {1})'.format(str(self.__f), str(self.__g))

    def __repr__(self):
        return '({0} / {1})'.format(
            repr(self.__f), repr(self.__g))


class _PowerFunction(Function):
    def __init__(self, f, g):
        assert isinstance(f, Function)
        assert isinstance(g, Function)
        self.__f = f
        self.__g = g

    def __call__(self, param):
        return self.__f(param) ** self.__g(param)

    def derivative(self):
        # Need natural log
        raise ValueError

    def __str__(self):
        return '({0} ** {1})'.format(str(self.__f), str(self.__g))

    def __repr__(self):
        return '({0} ** {1})'.format(repr(self.__f), repr(self.__g))


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
        return Function.constant(i.right - i.left) * \
            Function.identity() + Function.constant(i.left)

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
            self.assertEqual(val+34, (Function.constant(34) +
                                      Function.identity())(val))
        for i in self.intervals():
            self.assertEqual(i+i, (Function.identity() +
                                   Function.identity())(i))
        self.assertEqual(str(Function.constant(1) + Function.constant(2)),
                         '(1 + 2)')
        self.assertEqual(repr(Function.constant(1) + Function.constant(2)),
                         '(Function.constant(1) + Function.constant(2))')
        self.assertEqual((Function.identity() +
                          Function.constant(5)).derivative()(8), 1)
        self.numericalDerivativeTest(Function.identity() + Function.identity())
        self.numericalDerivativeTest(Function.identity() + \
                                         Function.constant(5))

    def test_product(self):
        for val in xrange(5):
            self.assertEqual(val*34, (Function.constant(34) *
                                      Function.identity())(val))
        for i in self.intervals():
            for j in self.intervals():
                self.assertEqual(i*j,
                                 (self.interval_function(i) *
                                  self.interval_function(j))
                                 (Interval(0, 1)))
        self.assertEqual(str(Function.constant(1) *
                             Function.constant(2)),
                         '(1 * 2)')
        self.assertEqual(repr(Function.constant(1) *
                              Function.constant(2)),
                         '(Function.constant(1) * Function.constant(2))')
        self.assertEqual((Function.identity() *
                          Function.constant(4)).derivative()(7), 4)
        self.numericalDerivativeTest(Function.identity() * Function.identity())
        self.numericalDerivativeTest(Function.identity() *
                                     Function.constant(5))

    def test_quotient(self):
        for v in xrange(5):
            for w in xrange(1,5):
                self.assertEqual(v/w, (Function.constant(v) /
                                       Function.identity())(w))
        self.assertEqual(str(Function.constant(1) / Function.constant(2)),
                         '(1 / 2)')
        self.assertEqual(repr(Function.constant(1) / Function.constant(2)),
                         '(Function.constant(1) / Function.constant(2))')
        # Floating point is exact for small powers of two
        self.assertEqual((Function.constant(1) /
                          Function.identity()).derivative()(2),
                         -0.25)
        self.numericalDerivativeTest(Function.constant(3) /
                                     Function.identity())
        self.numericalDerivativeTest(Function.identity() /
                                     Function.constant(5))

    def test_power(self):
        for val in xrange(5):
            self.assertEqual(34**val, (Function.constant(34) **
                                       Function.identity())(val))
        for val in xrange(5):
            self.assertEqual(val**34, (Function.identity() **
                                       Function.constant(34))(val))
        for i in self.intervals():
            try:
                expected = i**i
            except ValueError:
                continue
            self.assertEqual(expected, (Function.identity() **
                                        Function.identity())(i))
        self.assertEqual(str(Function.constant(1) ** Function.constant(2)),
                         '(1 ** 2)')
        self.assertEqual(repr(Function.constant(1) ** Function.constant(2)),
                         '(Function.constant(1) ** Function.constant(2))')
        # Need some tests of derivative, once it's implemented


if __name__ == '__main__':
    unittest.main()
