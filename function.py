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


class ConstFunction(Function):
    def __init__(self, k):
        self.__k = k

    def __call__(self, param):
        return self.__k

    def derivative(self):
        return ConstFunction(0)

    def __str__(self):
        return str(self.__k)

    def __repr__(self):
        return 'ConstFunction({0})'.format(repr(self.__k))


class IdentityFunction(Function):
    def __call__(self, param):
        return param

    def derivative(self):
        return ConstFunction(1)

    def __str__(self):
        return 'x'

    def __repr__(self):
        return 'IdentityFunction()'


class SumFunction(Function):
    def __init__(self, f, g):
        assert isinstance(f, Function)
        assert isinstance(g, Function)
        self.__f = f
        self.__g = g

    def __call__(self, param):
        return self.__f(param) + self.__g(param)

    def derivative(self):
        return SumFunction(self.__f.derivative(), self.__g.derivative())

    def __str__(self):
        return '({0} + {1})'.format(str(self.__f), str(self.__g))

    def __repr__(self):
        return 'SumFunction({0}, {1})'.format(repr(self.__f), repr(self.__g))


class ProductFunction(Function):
    def __init__(self, f, g):
        assert isinstance(f, Function)
        assert isinstance(g, Function)
        self.__f = f
        self.__g = g

    def __call__(self, param):
        return self.__f(param) * self.__g(param)

    def derivative(self):
        return SumFunction(ProductFunction(self.__f.derivative(), self.__g),
                           ProductFunction(self.__f, self.__g.derivative()))

    def __str__(self):
        return '({0} * {1})'.format(str(self.__f), str(self.__g))

    def __repr__(self):
        return 'ProductFunction({0}, {1})'.format(
            repr(self.__f), repr(self.__g))


class QuotientFunction(Function):
    def __init__(self, f, g):
        assert isinstance(f, Function)
        assert isinstance(g, Function)
        self.__f = f
        self.__g = g

    def __call__(self, param):
        # Do we want to convert ZeroDivisionError into ValueError here?
        return self.__f(param) / self.__g(param)

    def derivative(self):
        return QuotientFunction(SumFunction(
                ProductFunction(self.__f.derivative(), self.__g),
                ProductFunction(ConstFunction(-1),
                                ProductFunction(self.__f,
                                                self.__g.derivative()))),
                # this part should use PowerFunction
                ProductFunction(self.__g, self.__g))

    def __str__(self):
        return '({0} / {1})'.format(str(self.__f), str(self.__g))

    def __repr__(self):
        return 'QuotientFunction({0}, {1})'.format(
            repr(self.__f), repr(self.__g))


class PowerFunction(Function):
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
        return 'PowerFunction({0}, {1})'.format(repr(self.__f), repr(self.__g))


class _FunctionUnitTests(unittest.TestCase):
    def intervals(self):
        for p in xrange(-2, 3):
            for q in xrange(-2, 3):
                yield Interval(p, q)

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
            self.assertEqual(val, ConstFunction(val)(87))
            self.assertEqual(val, ConstFunction(val)(Interval(2,6)))
        self.assertEqual(str(ConstFunction(3)), '3')
        self.assertEqual(repr(ConstFunction(3)), 'ConstFunction(3)')
        self.assertEqual(ConstFunction(5).derivative()(3.67), 0)

    def test_identity(self):
        for val in xrange(5):
            self.assertEqual(val, IdentityFunction()(val))
        for i in self.intervals():
            self.assertEqual(i, IdentityFunction()(i))
        self.assertEqual(str(IdentityFunction()), 'x')
        self.assertEqual(repr(IdentityFunction()), 'IdentityFunction()')
        self.assertEqual(IdentityFunction().derivative()(6), 1)
        self.numericalDerivativeTest(IdentityFunction())

    def test_sum(self):
        for val in xrange(5):
            self.assertEqual(val+34, SumFunction(ConstFunction(34),
                                                 IdentityFunction())(val))
        for i in self.intervals():
            self.assertEqual(i+i, SumFunction(IdentityFunction(),
                                              IdentityFunction())(i))
        self.assertEqual(str(SumFunction(ConstFunction(1), ConstFunction(2))),
                         '(1 + 2)')
        self.assertEqual(repr(SumFunction(ConstFunction(1), ConstFunction(2))),
                         'SumFunction(ConstFunction(1), ConstFunction(2))')
        self.assertEqual(SumFunction(IdentityFunction(),
                                     ConstFunction(5)).derivative()(8), 1)
        self.numericalDerivativeTest(SumFunction(IdentityFunction(),
                                                 IdentityFunction()))
        self.numericalDerivativeTest(SumFunction(IdentityFunction(),
                                                 ConstFunction(5)))

    def test_product(self):
        for val in xrange(5):
            self.assertEqual(val*34, ProductFunction(ConstFunction(34),
                                                     IdentityFunction())(val))
        for i in self.intervals():
            for j in self.intervals():
                self.assertEqual(i*j, ProductFunction(ConstFunction(i),
                                                      ConstFunction(j))(0))
        self.assertEqual(str(ProductFunction(ConstFunction(1),
                                             ConstFunction(2))),
                         '(1 * 2)')
        self.assertEqual(repr(ProductFunction(ConstFunction(1),
                                              ConstFunction(2))),
                         'ProductFunction(ConstFunction(1), ConstFunction(2))')
        self.assertEqual(ProductFunction(IdentityFunction(),
                                         ConstFunction(4)).derivative()(7), 4)
        self.numericalDerivativeTest(ProductFunction(IdentityFunction(),
                                                     IdentityFunction()))
        self.numericalDerivativeTest(ProductFunction(IdentityFunction(),
                                                     ConstFunction(5)))

    def test_quotient(self):
        for v in xrange(5):
            for w in xrange(1,5):
                self.assertEqual(v/w, QuotientFunction(ConstFunction(v),
                                                       IdentityFunction())(w))
        self.assertEqual(str(QuotientFunction(ConstFunction(1),
                                              ConstFunction(2))),
                         '(1 / 2)')
        self.assertEqual(repr(QuotientFunction(ConstFunction(1),
                                               ConstFunction(2))),
                         'QuotientFunction(ConstFunction(1), ConstFunction(2))'
                         )
        # Floating point is exact for small powers of two
        self.assertEqual(QuotientFunction(ConstFunction(1),
                                          IdentityFunction()).derivative()(2),
                         -0.25)
        self.numericalDerivativeTest(QuotientFunction(ConstFunction(3),
                                                      IdentityFunction()))
        self.numericalDerivativeTest(QuotientFunction(IdentityFunction(),
                                                      ConstFunction(5)))

    def test_power(self):
        for val in xrange(5):
            self.assertEqual(34**val, PowerFunction(ConstFunction(34),
                                                    IdentityFunction())(val))
        for val in xrange(5):
            self.assertEqual(val**34, PowerFunction(IdentityFunction(),
                                                    ConstFunction(34))(val))
        for i in self.intervals():
            try:
                expected = i**i
            except ValueError:
                continue
            self.assertEqual(expected, PowerFunction(IdentityFunction(),
                                                     IdentityFunction())(i))
        self.assertEqual(str(PowerFunction(ConstFunction(1),
                                           ConstFunction(2))),
                         '(1 ** 2)')
        self.assertEqual(repr(PowerFunction(ConstFunction(1),
                                            ConstFunction(2))),
                         'PowerFunction(ConstFunction(1), ConstFunction(2))')
        # Need some tests of derivative, once it's implemented


if __name__ == '__main__':
    unittest.main()
