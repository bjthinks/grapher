from function import Function

f = Function.quotient(Function.constant(1), Function.identity())
for n in xrange(5):
    deriv = f
    for i in xrange(n):
        deriv = deriv.derivative()
    print 'f{0} = {1}'.format("'" * n, deriv)
