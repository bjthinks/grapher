from tokenize import *
import unittest


class Parse(object):
    def __init__(self, tokens):
        pass

    def go(self):
        pass


class _ParseUnitTests(unittest.TestCase):
    def test_basic_functions(self):
        Parse([Token('variable', 'x')]).go()


if __name__ == '__main__':
    unittest.main()
