import collections
import unittest
import re

# A token is a named tuple consisting of a type and a datum, as
# follows:
# Type:       Datum:
# 'number'    floating point value
# 'function'  string containing the function name
# 'variable'  string containing the variable name
# 'symbol'    one-character string containing the symbol
Token = collections.namedtuple('Token', 'type datum')

TRIG_FUNCTIONS = ['sin', 'cos', 'tan', 'cot', 'sec', 'csc']

INVERSE_TRIG_FUNCTIONS = ['a' + x for x in TRIG_FUNCTIONS]

LOG_FUNCTIONS = ['log', 'ln']

EXP_FUNCTIONS = ['exp']

FUNCTIONS = TRIG_FUNCTIONS + INVERSE_TRIG_FUNCTIONS + LOG_FUNCTIONS + EXP_FUNCTIONS

# A sequence of tuples (REGEXP, TOKEN_BUILDER), where REGEXP is the
# regular expression to match, and TOKEN_BUILDER is a function to call
# to transform a string into a token.
TOKENIZATION_RULES = ((re.compile(r'[0-9]+(\.[0-9]*)?|\.[0-9]+'), lambda s: Token('number', float(s))),
                      (re.compile('[a-zA-Z][a-zA-Z0-9]*'),
                       lambda s: Token('function' if s in FUNCTIONS else 'variable', s)),
                      (re.compile('[-()+*/^]'), lambda s: Token('symbol', s)))

WHITESPACE_REGEXP = re.compile('\s*')

# Yield a stream of tokens representing the given string.  If a
# tokenization error occurs while parsing, raise an exception.
def tokenize(input_str):
    pos = WHITESPACE_REGEXP.match(input_str).end()
    while pos < len(input_str):
        for regexp, token_builder in TOKENIZATION_RULES:
            m = regexp.match(input_str, pos)
            if m is not None:
                yield token_builder(m.group())
                pos = m.end()
                break
        else:
            raise Exception('Parse error at location {0}'.format(pos))
        pos = WHITESPACE_REGEXP.match(input_str, pos).end()


class _TokenizerUnitTests(unittest.TestCase):
    def should_succeed(self, input_str, expected_tokens):
        self.assertEqual(list(tokenize(input_str)), [Token(typ, datum) for typ, datum in expected_tokens])

    def should_fail(self, input_str):
        self.assertRaises(Exception, lambda x: list(tokenize(input_str)))

    def test_numbers(self):
        self.should_succeed("", [])
        self.should_fail(".")
        self.should_succeed("0.", [('number', 0.0)])
        self.should_succeed(".0", [('number', 0.0)])
        self.should_succeed("3.", [('number', 3.0)])
        self.should_succeed(".5", [('number', 0.5)])
        self.should_succeed("123.25", [('number', 123.25)])
        self.should_succeed("1.25.125", [('number', 1.25), ('number', .125)])
        self.should_succeed(".5.5", [('number', 0.5), ('number', 0.5)])
        self.should_succeed("3", [('number', 3.0)])

    def test_functions(self):
        for func in ["sin", "cos", "tan", "sec", "cot", "csc",
                     "asin", "acos", "atan", "asec", "acot", "acsc",
                     "log", "ln", "exp"]:
            self.should_succeed(func, [('function', func)])
            self.should_succeed(func + 'x', [('variable', func + 'x')])

    def test_variables(self):
        self.should_succeed('a', [('variable', 'a')])
        self.should_succeed('a3b45c6', [('variable', 'a3b45c6')])

    def test_symbols(self):
        for symbol in "+-*/()^":
            self.should_succeed(symbol, [('symbol', symbol)])
        for not_symbol in "!@#$%&_={}[]|:;<>,.?":
            self.should_fail(not_symbol)

if __name__ == '__main__':
    unittest.main()
