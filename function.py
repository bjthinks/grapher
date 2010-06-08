import abc
from interval import Interval
import unittest


class Function(object):
    __metaclass__ = abc.ABCMeta

    @abc.abstractmethod
    def __call__(self, param):
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

    def __str__(self):
        return str(self.__k)

    def __repr__(self):
        return 'ConstFunction({0})'.format(repr(self.__k))


class IdentityFunction(Function):
    def __call__(self, param):
        return param

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
        # Or is it time to invent our own exception, like a DomainError?
        # Should we use the same exception protocol for Interval & Function?
        return self.__f(param) / self.__g(param)

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

    def __str__(self):
        return '({0} ** {1})'.format(str(self.__f), str(self.__g))

    def __repr__(self):
        return 'PowerFunction({0}, {1})'.format(repr(self.__f), repr(self.__g))


class _FunctionUnitTests(unittest.TestCase):
    def intervals(self):
        for p in xrange(-2, 3):
            for q in xrange(-2, 3):
                yield Interval(p, q)

    def test_const(self):
        for val in xrange(5):
            self.assertEqual(val, ConstFunction(val)(87))
            self.assertEqual(val, ConstFunction(val)(Interval(2,6)))
        self.assertEqual(str(ConstFunction(3)), '3')
        self.assertEqual(repr(ConstFunction(3)), 'ConstFunction(3)')

    def test_identity(self):
        for val in xrange(5):
            self.assertEqual(val, IdentityFunction()(val))
        for i in self.intervals():
            self.assertEqual(i, IdentityFunction()(i))
        self.assertEqual(str(IdentityFunction()), 'x')
        self.assertEqual(repr(IdentityFunction()), 'IdentityFunction()')

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


if __name__ == '__main__':
    unittest.main()
