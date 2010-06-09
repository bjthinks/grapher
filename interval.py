from __future__ import division
from math import floor, log
import unittest

class Interval(object):

    def __init__(self, a, b = None):
        if b is None:
            if isinstance(a, Interval):
                left, right = a.left, a.right
            else:
                left, right = a, a
        else:
            left, right = a, b
        for v in (left, right):
            if not isinstance(v, (int, float, long)):
                raise TypeError
        self.__left, self.__right = sorted((left, right))

    @property
    def left(self):
        return self.__left

    @property
    def right(self):
        return self.__right

    @property
    def points(self):
        return (self.__left, self.__right)

    def __eq__(self, other):
        return isinstance(other, Interval) and self.left == other.left \
            and self.right == other.right

    def __str__(self):
        return '[{0}, {1}]'.format(self.left, self.right)

    def __repr__(self):
        return 'Interval{0}'.format((self.left, self.right))

    def __contains__(self, other):
        if not isinstance(other, (int, long, float)):
            raise TypeError
        return self.left <= other <= self.right

    def __pos__(self):
        return self

    def __neg__(self):
        return Interval(-self.left, -self.right)

    def __add__(self, other):
        if not isinstance(other, Interval):
            other = Interval(other)
        return Interval(self.left + other.left, self.right + other.right)

    def __radd__(self, other):
        return self + other

    def __sub__(self, other):
        if not isinstance(other, Interval):
            other = Interval(other)
        return Interval(self.left - other.right, self.right - other.left)

    def __rsub__(self, other):
        return (-self) + other

    def __mul__(self, other):
        if not isinstance(other, Interval):
            other = Interval(other)
        points = [a*b for a in self.points for b in other.points]
        return Interval(min(points), max(points))

    def __rmul__(self, other):
        return self * other

    def reciprocal(self):
        if 0 in self:
            raise ValueError
        # we are in __future__
        return Interval(1 / self.left, 1 / self.right)

    def __truediv__(self, other):
        if not isinstance(other, Interval):
            other = Interval(other)
        return self * other.reciprocal()

    def __rtruediv__(self, other):
        return self.reciprocal() * other

    def __pow__(self, other):
        # a**b == e**(b*ln a)
        if not isinstance(other, Interval):
            other = Interval(other)
        # Negative number to a fractional power is not defined
        if self.left < 0:
            if other.left != other.right or other.left != floor(other.left):
                raise ValueError
            # other is an integer
            other = long(other.left)
            # if other is zero, then result is Interval(1)
            if other == 0:
                return Interval(1)
            # if other is positive and odd, then result is
            # [self.left ** other, self.right ** other]
            # if other is positive and even, and self does not include 0,
            # then result is [self.right ** other, self.left ** other]
            elif other > 0:
                if other % 2 != 0 or 0 not in self:
                    return Interval(self.left ** other,
                                    self.right ** other)
            # if other is positive and even, and self does include 0,
            # then result is [0, max(self.left ** other, self.right ** other)]
                else:
                    return Interval(0, max(self.left ** other,
                                           self.right ** other))
            # if other is negative, result is 1 / (self ** -other)
            else:
                return 1 / (self ** -other)
        # self is nonnegative
        # we are guaranteed to be ok unless we have 0 ** negative
        if 0 in self and other.left < 0:
            raise ValueError
        points = [a**b for a in self.points for b in other.points]
        return Interval(min(points), max(points))

    def __rpow__(self, other):
        if not isinstance(other, Interval):
            other = Interval(other)
        return other.__pow__(self)

    def log(self):
        """Compute the natural log of the interval."""
        if self.left <= 0:
            # ln(x) is undefined iff x <= 0
            raise ValueError
        return Interval(log(self.left), log(self.right))


class intervalTest(unittest.TestCase):
    def pos_intervals(self):
        for p in xrange(0, 8):
            for q in xrange(0, 8):
                yield Interval(p / 2, q / 2)

    def test_basic(self):
        for left in (1, 2, 3):
            for right in (4,5,6):
                x = Interval(left, right)
                self.assertEqual(x.left, left)
                self.assertEqual(x.right, right)
                def f():
                    x.left = 5
                self.assertRaises(AttributeError, f)
                def g():
                    x.right = 5
                self.assertRaises(AttributeError, g)

                # equality
                self.assertEqual(Interval(left, right), Interval(left, right))
                self.assertEqual(Interval(left, right)
                                 == Interval(left, right + 1), False)
                self.assertEqual(Interval(left, right)
                                 == Interval(left + 1, right), False)
                self.assertEqual(x == '', False)

                # str
                self.assertEqual(str(Interval(left, right)),
                                 '[%s, %s]' % (left, right))

                # repr
                self.assertEqual(repr(x),
                                 'Interval({0}, {1})'.format(left, right))

        self.assertRaises(TypeError, Interval, 1, '')
        self.assertRaises(TypeError, Interval, '', 1)
        Interval(1.5, 2.5)
        Interval(100000000000000000000000, 100000000000000000000000000000)
        self.assertEqual(Interval(65,3).left, 3)

        self.assertEqual(Interval(7), Interval(7,7))
        self.assertEqual(Interval(Interval(1,2)), Interval(1,2))

    def test_contains(self):
        x = Interval(3, 5)
        for t, b in ((2, False), (3, True), (4, True), (5, True), (6, False)):
            self.assertEqual(t in x, b)
        self.assertRaises(TypeError, x.__contains__, x)

    def test_reciprocal(self):
        self.assertEqual(Interval(2,4).reciprocal(), Interval(.5, .25))
        self.assertEqual(Interval(-4,-8).reciprocal(), Interval(-.25, -.125))
        self.assertRaises(ValueError, Interval(-1, 1).reciprocal)

    def test_log(self):
        self.assertEqual(Interval(2,4).log(), Interval(log(2), log(4)))
        self.assertEqual(Interval(1,4).log(), Interval(0.0, log(4)))
        self.assertRaises(ValueError, Interval(0, 1).log)
        self.assertRaises(ValueError, Interval(-1, 1).log)
        self.assertRaises(ValueError, Interval(-2, -1).log)

    def test_ops(self):
        # unary ops
        self.assertEqual(+Interval(2,3), Interval(2,3))
        self.assertEqual(-Interval(2,3), Interval(-2,-3))

        # binary ops
        self.assertEqual(Interval(0,1) + Interval(0,1), Interval(0,2))
        self.assertEqual(Interval(0,2) + Interval(0,2), Interval(0,4))
        self.assertEqual(Interval(0,1) + Interval(0,2), Interval(0,3))
        self.assertEqual(Interval(1,2) + Interval(1,2), Interval(2,4))
        self.assertEqual(Interval(1,2) + 1, Interval(2,3))

        self.assertEqual(Interval(1,2) - Interval(1), Interval(0,1))
        self.assertEqual(Interval(1,2) * Interval(3,4), Interval(3,8))

        for op in ((lambda a, b: a + b),
                   (lambda a, b: a - b),
                   (lambda a, b: a * b),
                   (lambda a, b: a / b),
                   (lambda a, b: a ** b),
                   ):
            self.assertRaises(TypeError, op, Interval(1,2), '')
            for x in (1, Interval(2, 4), Interval(-2, -4), Interval(-2, 2)):
                for y in (1, Interval(2, 4), Interval(-2, -4), Interval(-2, 2)):
                    try:
                        z = op(x, y)
                        for p in range(-5, 5):
                            for q in range(-5, 5):
                                if p in Interval(x) and q in Interval(y):
                                    self.assertEqual(op(p, q) in Interval(op(x, y)), True)
                    except ValueError:
                        pass
        self.assertEqual(Interval(1,2) ** 2, Interval(1,4))
        for a in self.pos_intervals():
            for b in self.pos_intervals():
                result = a ** b
                self.assertTrue(a.left ** b.left in result)
                self.assertTrue(a.left ** b.right in result)
                self.assertTrue(a.right ** b.left in result)
                self.assertTrue(a.right ** b.right in result)
        self.assertRaises(ValueError, Interval(-1,-1).__pow__, Interval(1, 2))
        self.assertRaises(ValueError, Interval(-1,-1).__pow__, 1.5)
        self.assertEqual(Interval(-2,-0.5) ** 0, Interval(1,1))
        self.assertEqual(Interval(-2,2) ** 0, Interval(1,1))
        self.assertEqual(Interval(0.5,2) ** 0, Interval(1,1))
        self.assertEqual(Interval(-3, -2) ** 5, Interval(-3**5, -2**5))
        self.assertEqual(Interval(-3, -2) ** 4, Interval(2**4, 3**4))
        self.assertEqual(Interval(-3, -2) ** 3, Interval(-3**3, -2**3))
        self.assertEqual(Interval(-3, -2) ** 2, Interval(2**2, 3**2))
        self.assertEqual(Interval(-3, 3) ** 2, Interval(0, 3**2))
        self.assertEqual(Interval(-2, 3) ** 2, Interval(0, 3**2))
        self.assertEqual(Interval(-3, 2) ** 2, Interval(0, 3**2))
        self.assertEqual(Interval(-3, 3) ** 3, Interval(-3**3, 3**3))
        self.assertEqual(Interval(-2, 3) ** 3, Interval(-2**3, 3**3))
        self.assertEqual(Interval(-3, 2) ** 3, Interval(-3**3, 2**3))
        self.assertEqual(Interval(-3, -2) ** -5, Interval(-3**-5, -2**-5))
        self.assertEqual(Interval(-3, -2) ** -4, Interval(2**-4, 3**-4))
        self.assertEqual(Interval(-3, -2) ** -3, Interval(-3**-3, -2**-3))
        self.assertEqual(Interval(-3, -2) ** -2, Interval(2**-2, 3**-2))
        self.assertRaises(ValueError, Interval(-3, 3).__pow__, -2)
        self.assertRaises(ValueError, Interval(-2, 3).__pow__, -2)
        self.assertRaises(ValueError, Interval(-3, 2).__pow__, -2)
        self.assertRaises(ValueError, Interval(-3, 3).__pow__, -3)
        self.assertRaises(ValueError, Interval(-2, 3).__pow__, -3)
        self.assertRaises(ValueError, Interval(-3, 2).__pow__, -3)
        self.assertRaises(ValueError, Interval(0, 2).__pow__, Interval(-1, 2))
        self.assertRaises(ValueError, Interval(0, 2).__pow__, Interval(-2, -1))
        self.assertRaises(ValueError, Interval(-2, 2).__pow__, Interval(-1, 2))
        self.assertRaises(ValueError, Interval(-2, 2).__pow__, Interval(-2,-1))
        self.assertRaises(ValueError, Interval(0, 0).__pow__, Interval(-1, 2))
        self.assertRaises(ValueError, Interval(0, 0).__pow__, Interval(-2, -1))
        self.assertEqual(Interval(0, 0) ** Interval(0, 0), Interval(1, 1))


if __name__ == '__main__':
    unittest.main()
