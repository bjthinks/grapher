from function import Function
from interval import Interval

fs = [Function.quotient(Function.constant(1), Function.identity()),
      Function.power(Function.identity(), Function.constant(-1))]

for f in fs:
    for n in xrange(5):
        deriv_name = 'f' + "'"*n
        deriv = f
        for i in xrange(n):
            deriv = deriv.derivative().weak_simplify()
        print '{0} = {1}'.format(deriv_name, deriv)
        interval = Interval(1, 2)
        print '{0}({1}) = {2}'.format(deriv_name, interval, deriv(interval))
