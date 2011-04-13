from __future__ import division
import unittest
from function import Function
from interval import Interval


def is_bounded(function, x_interval, y_interval, depth = 12):
    '''Determine if a function is bounded by an interval, on an interval.'''

    if function(x_interval) in y_interval:
        return True
    if not function(x_interval.left) in y_interval:
        return False
    if not function(x_interval.right) in y_interval:
        return False
    # Under no circumstances should we recurse more than depth times
    if depth == 0:
        return None
    middle = sum(x_interval.points) / 2
    left_half = Interval(x_interval.left, middle)
    right_half = Interval(middle, x_interval.right)
    left_test = is_bounded(function, left_half, y_interval, depth - 1)
    if left_test == False:
        return False
    right_test = is_bounded(function, right_half, y_interval, depth - 1)
    if right_test == False:
        return False
    if left_test == True and right_test == True:
        return True
    return None


class _SlicingUnitTests(unittest.TestCase):

    def test_simple(self):
        self.assertTrue(is_bounded(Function.identity(), Interval(0,1),
                                   Interval(0,1)))
        self.assertFalse(is_bounded(Function.identity(), Interval(0,2),
                                    Interval(0,1)))
        self.assertFalse(is_bounded(Function.identity(), Interval(-1,1),
                                    Interval(0,1)))
        self.assertFalse(is_bounded(Function.identity(), Interval(-1,2),
                                    Interval(0,1)))
        self.assertTrue(is_bounded(Function.constant(0.), Interval(3,4),
                                   Interval(0,1)))
        self.assertFalse(is_bounded(Function.constant(2.), Interval(3,4),
                                    Interval(0,1)))

    def test_split(self):
        # If this ever breaks, we should solve the problem by writing a
        # new kind of Function that turns a lambda expression into a
        # Function.  That way, it will never be simplified.
        # f = x^2 + x
        f = Function.sum(Function.power(Function.identity(),
                                        Function.constant(2)),
                         Function.identity())
        # f(-1) = 0, f(0) = 0, f(-.5) = -.25
        # The range of f on [-1,0] is [-.25,0]
        # f([-1,0]) = [-1,1]
        # f([-1,-.5]) = [-.75,.5]
        # f([-.5,0]) = [-.5,.25]
        # This requires splitting in half
        self.assertTrue(is_bounded(f, Interval(-1,0), Interval(-3/4,1/2)))
        # This requires 3 levels of recursion
        self.assertTrue(is_bounded(f, Interval(-1,0), Interval(-3/8,1/8)))
        # This requires 8 levels of recursion
        self.assertTrue(is_bounded(f, Interval(-1,0), Interval(-65/256,1/256)))
        # This will never finish, since it asks for the exact bounds
        self.assertTrue(is_bounded(f, Interval(-1,0), Interval(-1/4,0)) is None)
        # g = 1/2*x^3-3/2*x
        g = Function.sum(Function.product(Function.constant(.5),
                                          Function.power(Function.identity(),
                                                         Function.constant(3))),
                         Function.product(Function.constant(-1.5),
                                          Function.identity()))
        self.assertEqual(is_bounded(g, Interval(-1.5,1.5), Interval(-.9,1)),
                         False)


if __name__ == '__main__':
    unittest.main()
