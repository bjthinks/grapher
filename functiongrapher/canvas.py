from __future__ import division
import unittest


class Line(object):

    def __init__(self, start, end):
        self.start = start
        self.end = end

    def svg(self, trans):
        x1, y1 = trans(self.start)
        x2, y2 = trans(self.end)
        return '<svg:line x1="{0}" y1="{1}" x2="{2}" y2="{3}" stroke="black" stroke-width="1"/>'.format(x1, y1, x2, y2)


class Circle(object):

    def __init__(self, center):
        self.center = center

    def svg(self, trans):
        cx, cy = trans(self.center)
        return '<svg:circle cx="{0}" cy="{1}" r="2" stroke="none" fill="black"/>'.format(cx, cy)


class Path(object):

    def __init__(self):
        self.points = []
        self.commands = []

    def line_to(self, end):
        self.points.append(end)
        self.commands.append('L')

    def move_to(self, end):
        self.points.append(end)
        self.commands.append('M')

    def spline_to(self, control_point_1, control_point_2, end):
        self.points.append((control_point_1, control_point_2, end))
        self.commands.append('C')

    @staticmethod
    def lines(start, *points):
        p = Path()
        p.move_to(start)
        for point in points:
            p.line_to(point)
        return p

    def svg(self, trans):
        cmd_strings = []
        for cmd, pts in zip(self.commands, self.points):
            if not isinstance(pts[0], tuple):
                pts = (pts,)
            cmd_strings.append(cmd + ' '.join(["{0} {1}".format(*trans(pt))
                                               for pt in pts]))
        d = ' '.join(cmd_strings)

        return '<svg:path d="{0}" stroke="black" stroke-width="1" fill="none"/>'.format(d)


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

    def add(self, element):
        self.elements.append(element)
        return self

    def output(self):
        return ['<svg:svg xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="{0}" height="{1}">'
                .format(self.__xpixels, self.__ypixels)] + \
                [e.svg(self.xy_to_pixels) for e in self.elements] + \
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
        line = Line((-6,10), (-2,12))
        self.assertEqual(line.svg(canvas.xy_to_pixels),
                         '<svg:line x1="0.0" y1="64.0" x2="128.0" y2="32.0" stroke="black" stroke-width="1"/>')
        self.assertEqual(line.start, (-6,10))
        self.assertEqual(line.end, (-2,12))

    def test_circle(self):
        canvas = Canvas(xpixels=256, ypixels=64, xmin=-6, xmax=2, ymin=10, ymax=14)
        circle = Circle((-6,10))
        self.assertEqual(circle.svg(canvas.xy_to_pixels),
                         '<svg:circle cx="0.0" cy="64.0" r="2" stroke="none" fill="black"/>')
        self.assertEqual(circle.center, (-6,10))

    def test_path_of_lines(self):
        canvas = Canvas(xpixels=256, ypixels=64, xmin=-6, xmax=2, ymin=10, ymax=14)
        path = Path()
        path.move_to((-6,10))
        path.line_to((-2,10))
        path.line_to((-2,12))
        self.assertEqual(path.svg(canvas.xy_to_pixels),
                         '<svg:path d="M0.0 64.0 L128.0 64.0 L128.0 32.0" stroke="black" stroke-width="1" fill="none"/>')
        self.assertEqual(path.points, [(-6,10), (-2,10), (-2,12)])
        self.assertEqual(path.commands, ['M', 'L', 'L'])

    def test_path_with_move(self):
        canvas = Canvas(xpixels=256, ypixels=64, xmin=-6, xmax=2, ymin=10, ymax=14)
        path = Path()
        path.move_to((-6,10))
        path.line_to((-2,10))
        path.move_to((-2,12))
        path.line_to((2,12))
        self.assertEqual(path.svg(canvas.xy_to_pixels),
                         '<svg:path d="M0.0 64.0 L128.0 64.0 M128.0 32.0 L256.0 32.0" stroke="black" stroke-width="1" fill="none"/>')
        self.assertEqual(path.points, [(-6,10), (-2,10), (-2,12), (2,12)])
        self.assertEqual(path.commands, ['M', 'L', 'M', 'L'])

    def test_path_with_spline(self):
        canvas = Canvas(xpixels=256, ypixels=64, xmin=-6, xmax=2, ymin=10, ymax=14)
        path = Path()
        path.move_to((-6,10))
        path.spline_to((-2,10), (-2,12), (2,12))
        self.assertEqual(path.svg(canvas.xy_to_pixels),
                         '<svg:path d="M0.0 64.0 C128.0 64.0 128.0 32.0 256.0 32.0" stroke="black" stroke-width="1" fill="none"/>')
        self.assertEqual(path.points, [(-6,10), ((-2,10), (-2,12), (2,12))])
        self.assertEqual(path.commands, ['M', 'C'])

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
        self.assertEqual(Canvas().add(Line((0,0), (1,1))).output(),
                         ['<svg:svg xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="500" height="500">',
                          '<svg:line x1="250.0" y1="250.0" x2="375.0" y2="125.0" stroke="black" stroke-width="1"/>',
                          '</svg:svg>'])
        self.assertEqual(Canvas().add(Line((-1,1), (2,2)))
                                 .add(Line((-2,-2), (2,0))).output(),
                         ['<svg:svg xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="500" height="500">',
                          '<svg:line x1="125.0" y1="125.0" x2="500.0" y2="0.0" stroke="black" stroke-width="1"/>',
                          '<svg:line x1="0.0" y1="500.0" x2="500.0" y2="250.0" stroke="black" stroke-width="1"/>',
                          '</svg:svg>'])

    def test_canvas_path(self):
        elements = Canvas().add(Path.lines((-1,1), (2,2), (-2,-2), (2,0))).elements
        self.assertEqual(len(elements), 1)
        self.assertEqual(type(elements[0]), Path)
        self.assertEqual(elements[0].points, [(-1,1), (2,2), (-2,-2), (2,0)])


if __name__ == '__main__':
    unittest.main()
