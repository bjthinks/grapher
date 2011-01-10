from __future__ import division
import unittest


class Canvas(object):

    # This class uses "fluent style" for chaining method invocations

    def __init__(self, xpixels = 500, ypixels = 500, xmin = -2, xmax = 2, ymin = -2, ymax = 2):
        self.__xpixels = xpixels
        self.__ypixels = ypixels
        self.__xmin = xmin
        self.__xmax = xmax
        self.__ymin = ymin
        self.__ymax = ymax
        self.__svg_output = []

    def line(self, start, end):
        # __svg_output stores 4-ples
        self.__svg_output.append(start + end)
        return self

    def lines(self, linelist):
        for line in linelist:
            self.line(*line)
        return self

    def path(self, pointlist):
        for i in xrange(len(pointlist)-1):
            self.line(pointlist[i], pointlist[i+1])
        return self

    def output(self):
        # someone has been writing TOO MUCH Haskell...
        return ['<svg:svg xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="{0}" height="{1}">'.format(self.__xpixels, self.__ypixels)] + \
            ['<svg:line x1="{0}" y1="{1}" x2="{2}" y2="{3}" stroke="black" stroke-width="1"/>'.format(self.__x_coord(x1), self.__y_coord(y1), self.__x_coord(x2), self.__y_coord(y2)) for (x1,y1,x2,y2) in self.__svg_output] + \
            ['</svg:svg>']

    def __x_coord(self, x):
        return str((x-self.__xmin)/(self.__xmax-self.__xmin)*self.__xpixels)

    def __y_coord(self, y):
        return str((self.__ymax-y)/(self.__ymax-self.__ymin)*self.__ypixels)


class canvasTest(unittest.TestCase):

    def test_create(self):
        Canvas()
        Canvas(xpixels=345, ypixels=654)
        Canvas(345, 654)
        Canvas(xmin=2, xmax=4)
        Canvas(ymin=2, ymax=4)
        Canvas(xpixels=456, ypixels=567, xmin=-6, xmax=83, ymin=2, ymax=4)
        Canvas(456, 567, -6, 83, 2, 4)

    def test_empty(self):
        self.assertEqual(Canvas().output(),
                         ['<svg:svg xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="500" height="500">',
                          '</svg:svg>'])
        self.assertEqual(Canvas(xpixels=200, ypixels=300).output(),
                         ['<svg:svg xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="200" height="300">',
                          '</svg:svg>'])

    def test_line(self):
        self.assertEqual(Canvas().line((0,0), (1,1)).output(),
                         ['<svg:svg xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="500" height="500">',
                          '<svg:line x1="250.0" y1="250.0" x2="375.0" y2="125.0" stroke="black" stroke-width="1"/>',
                          '</svg:svg>'])
        self.assertEqual(Canvas().line((-1,1), (2,2))
                                 .line((-2,-2), (2,0)).output(),
                         ['<svg:svg xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="500" height="500">',
                          '<svg:line x1="125.0" y1="125.0" x2="500.0" y2="0.0" stroke="black" stroke-width="1"/>',
                          '<svg:line x1="0.0" y1="500.0" x2="500.0" y2="250.0" stroke="black" stroke-width="1"/>',
                          '</svg:svg>'])

    def test_lines(self):
        self.assertEqual(Canvas().lines([((-1,1), (2,2)),((-2,-2), (2,0))])
                         .output(),
                         ['<svg:svg xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="500" height="500">',
                          '<svg:line x1="125.0" y1="125.0" x2="500.0" y2="0.0" stroke="black" stroke-width="1"/>',
                          '<svg:line x1="0.0" y1="500.0" x2="500.0" y2="250.0" stroke="black" stroke-width="1"/>',
                          '</svg:svg>'])


if __name__ == '__main__':
    unittest.main()
