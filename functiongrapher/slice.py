import unittest
from function import Function
from interval import Interval


def is_bounded(function, x_interval, y_interval):
    '''Determine if a function is bounded by an interval, on an interval.'''

    if function(x_interval) in y_interval:
        return True
    if not function(x_interval.left) in y_interval:
        return False
    if not function(x_interval.right) in y_interval:
        return False
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


if __name__ == '__main__':
    unittest.main()
