#!/usr/bin/env python

import wsgiref.handlers
import cgi
from google.appengine.ext import db
from google.appengine.ext import webapp
from google.appengine.ext.webapp import template

class MyHandler(webapp.RequestHandler):
    def get(self, *groups):
	if groups[0] == '' or groups[0] == 'index.html':
            self.response.headers['Content-Type'] = 'application/xhtml+xml'
	    function = self.request.get('f')
	    escaped_function = cgi.escape(function, True)
	    values = { 'escaped_function' : escaped_function,
                       'escaped_url' : cgi.escape(groups[0]),
                       'svg_graph' : graph(function) }
	    self.response.out.write(
		    template.render('main.html', values))
	else:
            self.error(404)
	    self.response.out.write(
		    template.render('error.html', {}))

def graph(function):
    output = '''\
<svg xmlns="http://www.w3.org/2000/svg" width="500" height="500" version="1.1">
  <g transform="translate(250,250) scale(1,-1)">
    <line x1="-250" y1="0" x2="250" y2="0" stroke="black" stroke-width="2"/>
    <line x1="-250" y1="0" x2="-240" y2="10" stroke="black" stroke-width="2"/>
    <line x1="-250" y1="0" x2="-240" y2="-10" stroke="black" stroke-width="2"/>
    <line x1="250" y1="0" x2="240" y2="10" stroke="black" stroke-width="2"/>
    <line x1="250" y1="0" x2="240" y2="-10" stroke="black" stroke-width="2"/>
    <line x1="0" y1="-250" x2="0" y2="250" stroke="black" stroke-width="2"/>
    <line x1="0" y1="-250" x2="-10" y2="-240" stroke="black" stroke-width="2"/>
    <line x1="0" y1="-250" x2="10" y2="-240" stroke="black" stroke-width="2"/>
    <line x1="0" y1="250" x2="-10" y2="240" stroke="black" stroke-width="2"/>
    <line x1="0" y1="250" x2="10" y2="240" stroke="black" stroke-width="2"/>
    <g transform="scale(250)">
      <line x1="-1" y1="-1" x2="1" y2="1" stroke="black" stroke-width=".004"/>
    </g>
  </g>
</svg>'''
    return output


def main():
    app = webapp.WSGIApplication([
		    (r'/(.*)', MyHandler)], debug=True)
    wsgiref.handlers.CGIHandler().run(app)

if __name__ == '__main__':
    main()
