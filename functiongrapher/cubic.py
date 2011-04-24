from __future__ import division
import unittest
from function import Function
from interval import Interval


class WrappedFunction(Function):
    def __init__(self, wrapped):
        self.__f = wrapped

    def __call__(self, param):
        return self.__f(param)

    def derivative(self):
        return self.__f.derivative()

    def polynomial_degree(self):
        return self.__f.polynomial_degree()

    def weak_simplify(self):
        return self.__f.weak_simplify()

    def __str__(self):
        return str(self.__f)

    def __repr__(self):
        return 'WrappedFunction({0})'.format(repr(self.__f))


class Cubic(WrappedFunction):
    def __init__(self, t0, t1, f0, c0, c1, f1):
        self.__t0 = t0
        self.__t1 = t1
        self.__f0 = f0
        self.__c0 = c0
        self.__c1 = c1
        self.__f1 = f1

        # The specific functional form will have consequences for the
        # efficiency of interval arithmetic (and thus, slicing).

        # t' = (t-t0)/(t1-t0)
        t = Function.product(Function.sum(Function.identity(), Function.constant(-t0)),
                             Function.constant(1.0/(t1-t0)))

        omt = Function.sum(Function.constant(1), Function.product(Function.constant(-1), t))
        def term(const, f):
            return Function.sum(Function.constant(const), Function.product(t, f))
        a = -f0 + 3*c0 - 3*c1 + f1
        b = 3*f0 - 6*c0 + 3*c1
        c = -3*f0 + 3*c0
        d = f0

        WrappedFunction.__init__(self, term(d, term(c, term(b, Function.constant(a)))).weak_simplify())

    @staticmethod
    def endpoint_slope_factory(t0, t1, f0, f1, d0, d1):
        c0 = f0 + d0 * (t1 - t0) / 3
        c1 = f1 - d1 * (t1 - t0) / 3
        return Cubic(t0, t1, f0, c0, c1, f1)

    @property
    def t0(self):
        return self.__t0

    @property
    def t1(self):
        return self.__t1

    @property
    def f0(self):
        return self.__f0

    @property
    def c0(self):
        return self.__c0

    @property
    def c1(self):
        return self.__c1

    @property
    def f1(self):
        return self.__f1


class _CubicUnitTests(unittest.TestCase):
    def test_zero(self):
        zero_cubic = Cubic(0, 1, 0, 0, 0, 0)
        self.assertEqual(zero_cubic(0), 0)
        self.assertEqual(zero_cubic(2), 0) # It's ok to evaluate outside the original domain
        self.assertEqual(zero_cubic(0.5), 0)
        self.assertEqual(zero_cubic(Interval(0, 1)), Interval(0))

    def test_line(self):
        line_cubic = Cubic(0, 1, 2, 4, 6, 8)
        self.assertEqual(line_cubic(0), 2)
        self.assertEqual(line_cubic(1), 8)
        self.assertEqual(line_cubic(0.5), 5)
        self.assertEqual(line_cubic(2), 14) # It's ok to evaluate outside the original domain
        self.assertEqual(line_cubic(Interval(0, 1)), Interval(2, 8))

    def test_translated_line(self):
        line_cubic = Cubic(2, 5, 2, 4, 6, 8)
        self.assertEqual(line_cubic(2), 2)
        self.assertEqual(line_cubic(3), 4)
        self.assertEqual(line_cubic(4), 6)
        self.assertEqual(line_cubic(5), 8)
        self.assertEqual(line_cubic(Interval(3, 4)), Interval(4, 6))

    def test_parabola(self):
        parabola = Cubic(0, 3, 0, 0, 3, 9)
        self.assertEqual(parabola(0), 0)
        self.assertEqual(parabola(1), 1)
        self.assertEqual(parabola(2), 4)
        self.assertEqual(parabola(3), 9)
        self.assertEqual(parabola(Interval(0, 3)), Interval(0, 9))

    def test_weird_cubic(self):
        # Make a cubic that passes through points (1, 0), (2, 1), (3, -1), (4, 0).
        # The control points for this are (1, 0), (2, 4.5), (3, -4.5), (4, 0).
        cubic = Cubic(1, 4, 0, 4.5, -4.5, 0)
        self.assertEqual(cubic(1), 0)
        self.assertEqual(cubic(2), 1)
        self.assertEqual(cubic(3), -1)
        self.assertEqual(cubic(4), 0)

    def test_control_point_read_back(self):
        cubic = Cubic(10, 20, 30, 40, 50, 60)
        self.assertEqual(cubic.t0, 10)
        self.assertEqual(cubic.t1, 20)
        self.assertEqual(cubic.f0, 30)
        self.assertEqual(cubic.c0, 40)
        self.assertEqual(cubic.c1, 50)
        self.assertEqual(cubic.f1, 60)

    def test_endpoint_slope_factory(self):
        # x^3 - x^2 + 2x + 1 on t = [0,1]
        # derivative: 3x^2 - 2x + 2
        cubic = Cubic.endpoint_slope_factory(0, 1, 1, 3, 2, 3)
        self.assertAlmostEqual(cubic(0), 1)
        self.assertAlmostEqual(cubic(1), 3)
        self.assertAlmostEqual(cubic(2), 9)
        self.assertAlmostEqual(cubic(3), 25)


if __name__ == '__main__':
    unittest.main()
