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
        # A straight line
        f0 = f(t0)
        f3 = f(t3)
        yield Cubic(t0, t3, f0, (2*f0 + f3)/3, (f0 + 2*f3)/3, f3)
    except ValueError:
        pass


class _ApproximationUnitTests(unittest.TestCase):

    def test_approx(self):
        f = Function.identity()
        for a in approximate(f, 0, 1):
            self.assertTrue(isinstance(a, Function))
            self.assertTrue(isinstance(a, Cubic))

        # (-x^2 - 1) ^ .5
        bad = Function.power(Function.sum(Function.product(
                Function.constant(-1),
                Function.power(Function.identity(), Function.constant(2))),
                                          Function.constant(-1)),
                             Function.constant(.5))
        self.assertEqual([a for a in approximate(bad, 0, 1)], [])


if __name__ == '__main__':
    unittest.main()
