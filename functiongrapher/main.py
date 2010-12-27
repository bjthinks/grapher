#!/usr/bin/env python

import wsgiref.handlers
import cgi
from google.appengine.ext import db
from google.appengine.ext import webapp
from google.appengine.ext.webapp import template
from canvas import Canvas
from tokenize import tokenize
from parse import Parse

class MyHandler(webapp.RequestHandler):
    def get(self, *groups):
	if groups[0] == '' or groups[0] == 'index.html':
            self.response.headers['Content-Type'] = 'application/xhtml+xml'
	    input = self.request.get('f')
	    escaped_input = cgi.escape(input, True)
            try:
                tokens = [t for t in tokenize(input)]
                try:
                    function = Parse(tokens).go()
                except (ParseError):
                    function = None
            except (Exception):
                tokens = None
                function = None
	    values = { 'escaped_input' : escaped_input,
                       'tokenized_function' : cgi.escape(str(tokens)),
                       'parsed_function' : cgi.escape(str(function)),
                       'escaped_url' : cgi.escape(groups[0]),
                       'svg_graph' : graph(function),
                       'lorem_ipsum' : lorem_ipsum()}
	    self.response.out.write(
		    template.render('main.html', values))
	else:
            self.error(404)
	    self.response.out.write(
		    template.render('error.html', {}))

def graph(f):
    linelist = [(-2,0,2,0), (-2,0,-1.92,.08), (-2,0,-1.92,-.08), (2,0,1.92,.08), (2,0,1.92,-0.08), (0,-2,0,2), (0,-2,-.08,-1.92), (0,-2,.08,-1.92), (0,2,-.08,1.92), (0,2,.08,1.92)]
    if f != None:
        dx = 0.1
        for x in [(x-20)/10. for x in xrange(40)]:
            try:
                linelist.append((x,f(x),x+dx,f(x+dx)))
            except (ZeroDivisionError, ValueError):
                pass
    return "\n".join(Canvas().lines(linelist).output())

def lorem_ipsum():
    return '''<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque libero tellus, condimentum a tempus vel, placerat ut orci. Suspendisse potenti. Ut aliquam aliquet tincidunt. Mauris sit amet nulla tristique dolor convallis faucibus quis eget mauris. Vestibulum facilisis, urna quis viverra sodales, ante sem dapibus metus, ut aliquam diam tortor eu tellus. Sed at ipsum id augue porta tempor. Mauris ornare, urna sit amet luctus adipiscing, sapien massa pretium enim, et congue nibh diam aliquet elit. Sed arcu libero, pellentesque ac iaculis sed, mattis non justo. Nulla mauris ligula, bibendum id tincidunt in, vulputate ut mi. Aenean malesuada placerat turpis et sollicitudin. Nulla rhoncus magna vel turpis eleifend eget tristique erat volutpat. Nunc sapien nunc, interdum ac dictum ac, eleifend nec diam. Nunc fringilla aliquam congue. Curabitur bibendum tellus in est vulputate sed cursus massa iaculis.</p>
<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec eu est diam. Aenean sapien eros, pellentesque a pulvinar vitae, sodales id diam. Mauris mattis leo pellentesque elit facilisis congue. Ut dui erat, lacinia sit amet rhoncus ut, lobortis ut elit. Mauris nec augue arcu. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed vitae magna diam. Pellentesque pellentesque, felis id laoreet semper, nibh magna tristique ante, a ornare nibh augue quis mi. Sed faucibus auctor felis vitae auctor.</p>
<p>Nunc libero quam, vehicula sed elementum ornare, eleifend non massa. Proin nulla urna, fermentum sit amet eleifend non, dapibus in lorem. Mauris lobortis congue diam, id accumsan quam pharetra eu. Aenean gravida, elit at luctus rutrum, velit eros ullamcorper orci, eget tristique ante eros sed ligula. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Suspendisse potenti. Proin aliquam sapien sed eros lobortis aliquet. Morbi cursus nunc ac sapien eleifend ut molestie magna tincidunt. Aenean malesuada consequat egestas. Sed viverra tincidunt scelerisque. Donec pellentesque, ante non hendrerit convallis, libero nunc dignissim risus, eu iaculis justo nisi et velit. Suspendisse at ultricies quam. Phasellus vitae magna eget urna gravida congue sodales at libero. Aliquam ullamcorper egestas elit, sit amet ultricies felis vulputate in. Maecenas pretium odio a est porta vel pulvinar est volutpat. Phasellus porttitor urna quis magna tristique ac vestibulum urna consectetur. Etiam rhoncus vehicula egestas. Nulla facilisi. Mauris lacinia tincidunt lacus quis vulputate.</p>
<p>Pellentesque in enim ante, at scelerisque purus. Duis sollicitudin interdum nibh, ac fringilla lacus pretium et. Curabitur felis velit, gravida eget suscipit id, interdum eu libero. Morbi blandit hendrerit nisi, vel venenatis mauris sollicitudin at. Nulla facilisi. Integer sit amet lectus est. Maecenas ultrices magna a mauris dignissim nec varius sapien dapibus. Fusce vitae nulla et ligula pretium mattis. Cras tristique orci nulla. Duis arcu massa, elementum in consectetur vitae, mollis eu lacus. Pellentesque sed odio erat. Praesent egestas, risus at hendrerit lacinia, ante sapien ullamcorper elit, a dignissim nibh dolor quis sem. Curabitur sit amet ipsum eget sapien faucibus iaculis elementum id lorem. Donec condimentum augue sed erat interdum vel dictum lectus accumsan.</p>
<p>Fusce ac nisi diam. Curabitur dignissim varius nulla, eget auctor sem sagittis nec. Cras aliquet suscipit laoreet. Curabitur lacinia sem vel quam euismod sit amet volutpat diam commodo. Nulla vulputate adipiscing magna, eu sodales urna consequat non. Aliquam quis nibh lorem. Aenean nec tellus felis. Integer in sem augue, et porta nunc. Nunc elit augue, pretium eget dictum vel, lacinia sed odio. Nam nisi tellus, sodales quis vulputate sit amet, vulputate at sem. Nunc a quam eros. In mauris velit, rutrum id tristique elementum, tristique non odio.</p>'''

def main():
    app = webapp.WSGIApplication([
		    (r'/(.*)', MyHandler)], debug=True)
    wsgiref.handlers.CGIHandler().run(app)

if __name__ == '__main__':
    main()
