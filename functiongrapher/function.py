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

    def polynomial_degree(self):
        """Return the degree of the polynomial, or None if not a polynomial."""
        return None

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
    def error():
        """Create a Function object representing an error condition.
        The resulting function will raise a ValueError when evaluated."""
        return _ErrorFunction()

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
    def product(*args):
        if not all(isinstance(f, Function) for f in args):
            raise ValueError
        # Expand any args that are _ProductFunctions into their terms.
        expanded_args = []
        for f in args:
            if isinstance(f, _ProductFunction):
                expanded_args.extend(f._terms)
            else:
                expanded_args.append(f)
        # Collect constants
        const_term = 1
        other_terms = []
        for f in expanded_args:
            if isinstance(f, _ConstantFunction):
                const_term *= f._k
            else:
                other_terms.append(f)
        # Do other simplifications
        if len(other_terms) == 0:
            return _ConstantFunction(const_term)
        if const_term != 1:
            other_terms.insert(0, _ConstantFunction(const_term))
        if len(other_terms) == 1:
            return other_terms[0]
        return _ProductFunction(tuple(other_terms))

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
            try:
                return Function.constant(f._k ** g._k)
            except (ValueError, ZeroDivisionError):
                return Function.error()
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

    @staticmethod
    def sin(f):
        if not isinstance(f, Function):
            raise ValueError
        return _SinFunction(f)

    @staticmethod
    def cos(f):
        if not isinstance(f, Function):
            raise ValueError
        return _CosFunction(f)


class _ErrorFunction(Function):
    def __call__(self, param):
        raise ValueError

    def derivative(self):
        return self

    def polynomial_degree(self):
        return None

    def weak_simplify(self):
        return self

    def __str__(self):
        return '<error>'

    def __repr__(self):
        return 'Function.error()'


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

    def polynomial_degree(self):
        return 0

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

    def polynomial_degree(self):
        return 1

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

    def polynomial_degree(self):
        summand_degrees = [ term.polynomial_degree() for term in self._terms ]
        if None in summand_degrees:
            return None
        else:
            return max(summand_degrees)

    def weak_simplify(self):
        return Function.sum(*[f.weak_simplify() for f in self._terms])

    def __str__(self):
        return '({0})'.format(' + '.join(str(f) for f in self._terms))

    def __repr__(self):
        return 'Function.sum({0})'.format(
            ', '.join(repr(f) for f in self._terms))


class _ProductFunction(Function):
    def __init__(self, terms):
        assert isinstance(terms, tuple)
        assert len(terms) >= 2
        assert all(isinstance(f, Function) for f in terms)
        self._terms = terms

    def __call__(self, param):
        evaluated_terms = [f(param) for f in self._terms]
        result = evaluated_terms[0]
        for i in xrange(1, len(evaluated_terms)):
            result *= evaluated_terms[i]
        return result

    def derivative(self):
        term_range = xrange(len(self._terms))
        return Function.sum(
            *[Function.product(
                    *[(self._terms[j].derivative() if i == j
                       else self._terms[j])
                      for j in term_range])
              for i in term_range])

    def polynomial_degree(self):
        summand_degrees = [ term.polynomial_degree() for term in self._terms ]
        if None in summand_degrees:
            return None
        else:
            return sum(summand_degrees)

    def weak_simplify(self):
        result = Function.product(*[f.weak_simplify() for f in self._terms])
        if isinstance(result, _ProductFunction) \
                and isinstance(result._terms[0], _ConstantFunction) \
                and result._terms[0]._k == 0:
            return result._terms[0]
        return result

    def __str__(self):
        return '({0})'.format(' * '.join(str(f) for f in self._terms))

    def __repr__(self):
        return 'Function.product({0})'.format(', '.join(repr(f) for f in self._terms))


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

    def polynomial_degree(self):
        if isinstance(self.__g, _ConstantFunction):
            return self.__f.polynomial_degree()
        else:
            return None

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
        if isinstance(self._g, _ConstantFunction):
            # D(f(x) ** k) == k * f(x) ** (k-1) * Df(x)
            return Function.product(Function.constant(self._g._k),
                                    Function.power(self._f, Function.constant(self._g._k - 1)), self._f.derivative())
        # f(x) ** g(x) * Dg(x) * log(f(x)) + f(x) ** (g(x) - 1) * g(x) * Df(x)
        return Function.sum(
            Function.product(self,
                             Function.product(self._g.derivative(),
                                              Function.log(self._f))),
            Function.product(Function.power(
                    self._f, Function.sum(self._g, Function.constant(-1))),
                             Function.product(self._g, self._f.derivative())))

    def polynomial_degree(self):
        base_degree = self._f.polynomial_degree()
        if base_degree == None:
            return None
        if not isinstance(self._g, _ConstantFunction):
            return None
        exponent = self._g._k
        if exponent % 1 == 0 and exponent >= 0:
            return base_degree * exponent
        else:
            return None

    def weak_simplify(self):
        f = self._f.weak_simplify()
        g = self._g.weak_simplify()
        if isinstance(g, _ConstantFunction) and g._k == 0:
            return Function.constant(1)
        if isinstance(f, _ConstantFunction) and f._k == 1:
            return f
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


class _SinFunction(Function):
    def __init__(self, f):
        assert isinstance(f, Function)
        self.__f = f

    def __call__(self, param):
        f_value = self.__f(param)
        if isinstance(f_value, Interval):
            return Interval.sin(f_value)
        else:
            return math.sin(f_value)

    def derivative(self):
        return Function.product(Function.cos(self.__f), self.__f.derivative())

    def weak_simplify(self):
        f = self.__f.weak_simplify()
        return Function.sin(f)

    def __str__(self):
        return 'sin({0})'.format(self.__f)

    def __repr__(self):
        return 'Function.sin({0})'.format(repr(self.__f))


class _CosFunction(Function):
    def __init__(self, f):
        assert isinstance(f, Function)
        self.__f = f

    def __call__(self, param):
        f_value = self.__f(param)
        if isinstance(f_value, Interval):
            return Interval.cos(f_value)
        else:
            return math.cos(f_value)

    def derivative(self):
        return Function.product(Function.product(
                Function.constant(-1), Function.sin(self.__f)),
                                self.__f.derivative())

    def weak_simplify(self):
        f = self.__f.weak_simplify()
        return Function.cos(f)

    def __str__(self):
        return 'cos({0})'.format(self.__f)

    def __repr__(self):
        return 'Function.cos({0})'.format(repr(self.__f))


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

    def numericalDerivativeTest(self, f,
                                points = [2.74557456, 1.1436346, 1.743563456]):
        h = 1e-6
        tol = 1e-4
        deriv = f.derivative()
        for x in points:
            v1 = deriv(x)
            v2 = (f(x+h/2) - f(x-h/2)) / h
            # We want v1 and v2 to be within tol of each other
            self.assertTrue(abs(v1-v2) / abs(v1+v2) < tol)

    def test_error(self):
        f = Function.error()
        self.assertTrue(isinstance(f, _ErrorFunction))
        self.assertRaises(ValueError, f, 0)
        self.assertTrue(isinstance(f.derivative(), _ErrorFunction))
        self.assertEqual(str(f), '<error>')
        self.assertEqual(repr(f), 'Function.error()')

    def test_const(self):
        for val in xrange(5):
            self.assertEqual(val, Function.constant(val)(87))
            self.assertEqual(val, Function.constant(val)(Interval(2,6)))
            self.assertEqual(str(Function.constant(val).weak_simplify()), str(val))
        self.assertEqual(str(Function.constant(3)), '3')
        self.assertEqual(repr(Function.constant(3)), 'Function.constant(3)')
        self.assertEqual(Function.constant(5).derivative()(3.67), 0)
        self.assertEqual(Function.constant(17).polynomial_degree(), 0)

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
        self.assertEqual(Function.identity().polynomial_degree(), 1)

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
        self.assertEqual(Function.sum(Function.constant(3),
                                      Function.identity()).polynomial_degree(), 1)
        self.assertEqual(Function.sum(Function.constant(3),
                                      Function.product(Function.identity(),
                                                       Function.identity()))
                         .polynomial_degree(), 2)

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
                         '(2 * x)')
        self.assertEqual(repr(Function.product(Function.identity(),
                                               Function.constant(2))),
                         'Function.product(Function.constant(2), ' +
                         'Function.identity())')
        self.assertEqual(Function.product(
                Function.identity(), Function.constant(4)).derivative()(7), 4)
        self.numericalDerivativeTest(Function.product(Function.identity(),
                                                      Function.identity()))
        self.numericalDerivativeTest(Function.product(Function.identity(),
                                                      Function.constant(5)))
        c = Function.constant
        x = Function.identity()
        prod = Function.product
        log = Function.log
        pow = Function.power
        # Variable argument count
        self.assertEqual(str(Function.product()), '1')
        self.assertEqual(str(Function.product(c(3))), '3')
        self.assertEqual(str(Function.product(x)), 'x')
        f = Function.product(c(3), x, log(x))
        self.assertEqual(str(f), '(3 * x * ln(x))')
        self.assertEqual(repr(f),
                         'Function.product(Function.constant(3), '
                         'Function.identity(), '
                         'Function.log(Function.identity()))')
        self.assertEqual(f(2), 3 * 2 * math.log(2))
        self.numericalDerivativeTest(f)
        # Simplifications:
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
        # (x*2)*3 = 6*x
        self.assertEqual(str(prod(prod(x, c(2)), c(3))), '(6 * x)')
        self.assertEqual(Function.product(Function.identity(),
                                          Function.identity(),
                                          Function.power(Function.identity(),
                                                         Function.constant(2)))
                         .polynomial_degree(), 4)

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
            '(((2 * x * (1 + (x ** 3)))'
            ' + (-3 * (1 + (x ** 2)) * (x ** 2)))'
            ' / ((1 + (x ** 3)) ** 2))')
        # Simplifications
        # (1 + x*0)/(2*x) = 1/(2*x)
        f = Function.quotient(plus(c(1), prod(x, c(0))), prod(c(2), x))
        self.assertEqual(str(f.weak_simplify()), '(1 / (2 * x))')
        # 1/(x + x*0) = 1/x
        f = Function.quotient(c(1), plus(x, prod(x, c(0))))
        self.assertEqual(str(f.weak_simplify()), '(1 / x)')
        self.assertEqual(Function.quotient(Function.identity(),
                                           Function.identity()).polynomial_degree(), None)
        self.assertEqual(Function.quotient(Function.identity(),
                                           Function.constant(3)).polynomial_degree(), 1)
        self.assertEqual(Function.quotient(Function.constant(7),
                                           Function.constant(3)).polynomial_degree(), 0)

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
            if v == round(v):
                self.numericalDerivativeTest(Function.power(
                        Function.identity(), Function.constant(v)),
                                             points = [-1.3483255])
            self.numericalDerivativeTest(Function.power(
                    Function.constant(v), Function.identity()))
            self.numericalDerivativeTest(Function.power(
                    Function.constant(v), Function.identity()),
                                         points = [-1.3483255])
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
        self.assertEqual(Function.power(Function.identity(), Function.constant(7))
                         .polynomial_degree(), 7)
        self.assertEqual(Function.power(Function.constant(2), Function.identity())
                         .polynomial_degree(), None)

    def test_power_consts(self):
        # Test that power operates correctly when raising a const to a
        # const, especically in the presence of exceptions.
        values = [-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2]
        for x in values:
            for y in values:
                try:
                    z = x ** y
                except (ValueError, ZeroDivisionError):
                    z = None
                f = Function.power(Function.constant(x), Function.constant(y))
                if z is None:
                    self.assertTrue(isinstance(f, _ErrorFunction))
                else:
                    self.assertTrue(isinstance(f, _ConstantFunction))
                    self.assertEqual(f._k, z)
        self.assertEqual(Function.power(Function.constant(2), Function.constant(2))
                         .polynomial_degree(), 0)

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
        self.assertEqual(Function.log(Function.identity()).polynomial_degree(), None)

    def test_sine(self):
        for val in xrange(1, 5):
            self.assertEqual(math.sin(val),
                             Function.sin(Function.identity())(val))
        for i in self.intervals():
            expected = i.sin()
            self.assertEqual(expected,
                             Function.sin(Function.identity())(i))
        self.assertEqual(str(Function.sin(Function.constant(5))),
                         'sin(5)')
        self.assertEqual(repr(Function.sin(Function.constant(5))),
                         'Function.sin(Function.constant(5))')
        self.numericalDerivativeTest(Function.sin(Function.identity()))
        self.numericalDerivativeTest(
            Function.sin(
                Function.sum(Function.identity(), Function.constant(1))))
        self.numericalDerivativeTest(
            Function.sin(
                Function.product(Function.identity(), Function.constant(2))))
        # Simplifications
        c = Function.constant
        x = Function.identity()
        prod = Function.product
        sum = Function.sum
        # sin (x + x*0) = sin x
        f = Function.sin(sum(x, prod(x, c(0))))
        self.assertEqual(str(f.weak_simplify()), 'sin(x)')
        self.assertEqual(Function.sin(Function.identity()).polynomial_degree(), None)

    def test_cosine(self):
        for val in xrange(1, 5):
            self.assertEqual(math.cos(val),
                             Function.cos(Function.identity())(val))
        for i in self.intervals():
            expected = i.cos()
            self.assertEqual(expected,
                             Function.cos(Function.identity())(i))
        self.assertEqual(str(Function.cos(Function.constant(5))),
                         'cos(5)')
        self.assertEqual(repr(Function.cos(Function.constant(5))),
                         'Function.cos(Function.constant(5))')
        self.numericalDerivativeTest(Function.cos(Function.identity()))
        self.numericalDerivativeTest(
            Function.cos(
                Function.sum(Function.identity(), Function.constant(1))))
        self.numericalDerivativeTest(
            Function.cos(
                Function.product(Function.identity(), Function.constant(2))))
        # Simplifications
        c = Function.constant
        x = Function.identity()
        prod = Function.product
        sum = Function.sum
        # cos (x + x*0) = cos x
        f = Function.cos(sum(x, prod(x, c(0))))
        self.assertEqual(str(f.weak_simplify()), 'cos(x)')
        self.assertEqual(Function.cos(Function.identity()).polynomial_degree(), None)

if __name__ == '__main__':
    unittest.main()
