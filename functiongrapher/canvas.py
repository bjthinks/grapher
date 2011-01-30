from __future__ import division
import unittest


class Line(object):

    def __init__(self, trans, start, end):
        self.trans = trans
        self.start = start
        self.end = end

    def __str__(self):
        x1, y1 = self.trans(self.start)
        x2, y2 = self.trans(self.end)
        return '<svg:line x1="{0}" y1="{1}" x2="{2}" y2="{3}" stroke="black" stroke-width="1"/>'.format(x1, y1, x2, y2)


class Path(object):

    def __init__(self, trans, start):
        self.trans = trans
        self.points = [start]

    def line_to(self, end):
        self.points.append(end)

    def __str__(self):
        self.d = "M{0} {1}".format(*self.trans(self.points[0]))
        for i in xrange(1,len(self.points)):
            self.d += " L{0} {1}".format(*self.trans(self.points[i]))
        return '<svg:path d="{0}" stroke="black" stroke-width="1" fill="none"/>'.format(self.d)


class Canvas(object):

    # This class uses "fluent style" for chaining method invocations

    def __init__(self, xpixels = 500, ypixels = 500, xmin = -2, xmax = 2, ymin = -2, ymax = 2):
        self.__xpixels = xpixels
        self.__ypixels = ypixels
        self.__xmin = xmin
        self.__xmax = xmax
        self.__ymin = ymin
        self.__ymax = ymax
        self.elements = []

    def line(self, start, end):
        # __svg_output stores drawable objects
        self.elements.append(Line(self.xy_to_pixels, start, end))
        return self

    def lines(self, linelist):
        for line in linelist:
            self.line(*line)
        return self

    def path(self, pointlist):
        p = Path(self.xy_to_pixels, pointlist[0])
        for i in xrange(1,len(pointlist)):
            p.line_to(pointlist[i])
        self.elements.append(p)
        return self

    def output(self):
        return ['<svg:svg xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="{0}" height="{1}">'
                .format(self.__xpixels, self.__ypixels)] + \
                [str(e) for e in self.elements] + \
            ['</svg:svg>']

    def xy_to_pixels(self, xy_point):
        return (self.__x_coord(xy_point[0]), self.__y_coord(xy_point[1]))

    def __x_coord(self, x):
        return (x-self.__xmin)/(self.__xmax-self.__xmin)*self.__xpixels

    def __y_coord(self, y):
        return (self.__ymax-y)/(self.__ymax-self.__ymin)*self.__ypixels


class canvasTest(unittest.TestCase):

    def test_coordinate_transformation(self):
        canvas = Canvas(xpixels=256, ypixels=64, xmin=-6, xmax=2, ymin=10, ymax=14)
        self.assertEqual(canvas.xy_to_pixels((-6, 10)), (0, 64))
        self.assertEqual(canvas.xy_to_pixels((2, 14)), (256, 0))

    def test_line(self):
        canvas = Canvas(xpixels=256, ypixels=64, xmin=-6, xmax=2, ymin=10, ymax=14)
        line = Line(canvas.xy_to_pixels, (-6,10), (-2,12))
        self.assertEqual(str(line),
                         '<svg:line x1="0.0" y1="64.0" x2="128.0" y2="32.0" stroke="black" stroke-width="1"/>')
        self.assertEqual(line.start, (-6,10))
        self.assertEqual(line.end, (-2,12))

    def test_path(self):
        canvas = Canvas(xpixels=256, ypixels=64, xmin=-6, xmax=2, ymin=10, ymax=14)
        path = Path(canvas.xy_to_pixels, (-6,10))
        path.line_to((-2,10))
        path.line_to((-2,12))
        self.assertEqual(str(path),
                         '<svg:path d="M0.0 64.0 L128.0 64.0 L128.0 32.0" stroke="black" stroke-width="1" fill="none"/>')
        self.assertEqual(path.points, [(-6,10), (-2,10), (-2,12)])

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

    def test_canvas_line(self):
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

    def test_canvas_lines(self):
        self.assertEqual(Canvas().lines([((-1,1), (2,2)),((-2,-2), (2,0))])
                         .output(),
                         ['<svg:svg xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="500" height="500">',
                          '<svg:line x1="125.0" y1="125.0" x2="500.0" y2="0.0" stroke="black" stroke-width="1"/>',
                          '<svg:line x1="0.0" y1="500.0" x2="500.0" y2="250.0" stroke="black" stroke-width="1"/>',
                          '</svg:svg>'])

    def test_canvas_path(self):
        elements = Canvas().path([(-1,1), (2,2), (-2,-2), (2,0)]).elements
        self.assertEqual(len(elements), 1)
        self.assertEqual(type(elements[0]), Path)
        self.assertEqual(elements[0].points, [(-1,1), (2,2), (-2,-2), (2,0)])


if __name__ == '__main__':
    unittest.main()
