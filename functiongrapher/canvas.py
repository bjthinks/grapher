from __future__ import division
import unittest


class Canvas(object):

    def __init__(self, xsize = 500, ysize = 500, xmin = -2, xmax = 2, ymin = -2, ymax = 2):
        self.__xsize = xsize
        self.__ysize = ysize
        self.__xmin = xmin
        self.__xmax = xmax
        self.__ymin = ymin
        self.__ymax = ymax
        self.__lines = []

    def line(self, x1, y1, x2, y2):
        self.__lines.append((x1,y1,x2,y2))
        return self

    def output(self):
        # someone has been writing TOO MUCH Haskell...
        return ['<svg:svg xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="' + str(self.__xsize) + '" height="' + str(self.__ysize) + '">'] + \
            ['<svg:line x1="' + self.__x_coord(x1) + '" y1="' + self.__y_coord(y1) + '" x2="' + self.__x_coord(x2) + '" y2="' + self.__y_coord(y2) + '" stroke="black" stroke-width="1"/>' for (x1,y1,x2,y2) in self.__lines] + \
            ['</svg:svg>']

    def __x_coord(self, x):
        return str((x-self.__xmin)/(self.__xmax-self.__xmin)*self.__xsize)

    def __y_coord(self, y):
        return str((self.__ymax-y)/(self.__ymax-self.__ymin)*self.__ysize)


class canvasTest(unittest.TestCase):

    def test_create(self):
        Canvas()
        Canvas(xsize=345, ysize=654)
        Canvas(xmin=2, xmax=4)
        Canvas(ymin=2, ymax=4)
        Canvas(xsize=456, ysize=567, xmin=-6, xmax=83, ymin=2, ymax=4)

    def test_empty(self):
        self.assertEqual(Canvas().output(),
                         ['<svg:svg xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="500" height="500">',
                          '</svg:svg>'])
        self.assertEqual(Canvas(xsize=200, ysize=300).output(),
                         ['<svg:svg xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="200" height="300">',
                          '</svg:svg>'])

    def test_line(self):
        self.assertEqual(Canvas().line(x1=0,y1=0,x2=1,y2=1).output(),
                         ['<svg:svg xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="500" height="500">',
                          '<svg:line x1="250.0" y1="250.0" x2="375.0" y2="125.0" stroke="black" stroke-width="1"/>',
                          '</svg:svg>'])


if __name__ == '__main__':
    unittest.main()
