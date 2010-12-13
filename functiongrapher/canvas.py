import unittest


class Canvas(object):

    def __init__(self, xsize = 500, ysize = 500, xmin = -2, xmax = 2, ymin = -2, ymax = 2):
        self.__xsize = xsize
        self.__ysize = ysize

    def output(self):
        return ['<svg:svg xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="' + str(self.__xsize) + '" height="' + str(self.__ysize) + '">',
                '</svg:svg>']


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


if __name__ == '__main__':
    unittest.main()
