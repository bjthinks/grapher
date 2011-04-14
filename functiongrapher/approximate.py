from __future__ import division
import unittest
from function import Function
from interval import Interval
from cubic import Cubic


def approximate(f, t0, t3 = None):
    if isinstance(t0, Interval) and t3 is None:
        (t0, t3) = t0.points

    # Each approximation lives in its own try-except block

    try:
        yield cubic_derivative_approximation(f, t0, t3)
    except ValueError:
        pass

    try:
        yield linear_approximation(f, t0, t3)
    except ValueError:
        pass


def cubic_derivative_approximation(f, t0, t3):
    f0 = f(t0)
    f3 = f(t3)
    diff_f = f.derivative()
    diff_f0 = diff_f(t0)
    diff_f3 = diff_f(t3)
    f1 = f0 + diff_f0 * (t3 - t0) / 3
    f2 = f3 - diff_f3 * (t3 - t0) / 3
    return Cubic(t0, t3, f0, f1, f2, f3)


def linear_approximation(f, t0, t3):
    f0 = f(t0)
    f3 = f(t3)
    return Cubic(t0, t3, f0, (2*f0 + f3)/3, (f0 + 2*f3)/3, f3)


class _ApproximationUnitTests(unittest.TestCase):

    def test_approx(self):
        f = Function.identity()
        for a in approximate(f, 0, 1):
            self.assertTrue(isinstance(a, Function))
            self.assertTrue(isinstance(a, Cubic))
        for a in approximate(f, Interval(0, 1)):
            self.assertTrue(isinstance(a, Function))
            self.assertTrue(isinstance(a, Cubic))
            # We may someday generate approximations that don't go through
            # the endpoints, and remove these tests.  Until then, they
            # help verify that the approximation formulas are correct.
            self.assertEqual(a(0.0), 0.0)
            self.assertEqual(a(1.0), 1.0)

        # (-x^2 - 1) ^ .5
        bad = Function.power(Function.sum(Function.product(
                Function.constant(-1),
                Function.power(Function.identity(), Function.constant(2))),
                                          Function.constant(-1)),
                             Function.constant(.5))
        self.assertEqual([a for a in approximate(bad, 0, 1)], [])

    def test_cubic_derivative(self):
        a = cubic_derivative_approximation(
            Function.power(Function.identity(), Function.constant(3)),
            4, 10)
        self.assertAlmostEqual(a(0), 0)
        self.assertAlmostEqual(a(2), 8)
        self.assertAlmostEqual(a(5), 125)
        self.assertAlmostEqual(a(12), 1728)
        self.assertAlmostEqual(a(-3), -27)

    def test_linear(self):
        a = linear_approximation(Function.identity(), 0, 1)
        self.assertEqual(a(3), 3)
        self.assertEqual(a(44), 44)


if __name__ == '__main__':
    unittest.main()
