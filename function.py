import abc
import unittest


class Function(object):
    __metaclass__ = abc.ABCMeta

    @abc.abstractmethod
    def __call__(self, param):
        pass


class ConstFunction(Function):
    def __init__(self, k):
        self.__k = k

    def __call__(self, param):
        return self.__k


class IdentityFunction(Function):
    def __call__(self, param):
        return param


class SumFunction(Function):
    def __init__(self, f, g):
        assert isinstance(f, Function)
        assert isinstance(g, Function)
        self.__f = f
        self.__g = g

    def __call__(self, param):
        return self.__f(param) + self.__g(param)


class _FunctionUnitTests(unittest.TestCase):
    def test_const(self):
        for val in xrange(5):
            self.assertEqual(val, ConstFunction(val)(87))

    def test_identity(self):
        for val in xrange(5):
            self.assertEqual(val, IdentityFunction()(val))

    def test_sum(self):
        for val in xrange(5):
            self.assertEqual(val+34, SumFunction(ConstFunction(34), IdentityFunction())(val))


if __name__ == '__main__':
    unittest.main()
