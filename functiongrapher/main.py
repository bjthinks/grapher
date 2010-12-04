#!/usr/bin/env python

import wsgiref.handlers
import cgi
from google.appengine.ext import db
from google.appengine.ext import webapp
from google.appengine.ext.webapp import template

class MyHandler(webapp.RequestHandler):
	def get(self, *groups):
		if groups[0] == 'favicon.ico':
			self.response.out.write(template.render('favicon.ico', {}))
                # elif groups[0] == 'the_image.svg':
                #         self.response.out.write(template.render('the_image.svg', {}))
                elif groups[0] == '' or groups[0] == 'index.html':
			self.response.headers['Content-Type'] = 'application/xhtml+xml'
			function = self.request.get('f')
			escaped_function = cgi.escape(function, True)
                        values = { 'escaped_function' : escaped_function }
                        self.response.out.write(
                                template.render('main.html', values))
                        self.response.out.write(
                                template.render('the_image.svg', values))
                        self.response.out.write('<p>You accessed this page at the URL: /' + groups[0] + "</p>")
			self.response.out.write(
				template.render('footer.html', values))
		else:
			self.error(404)
			self.response.out.write(
				template.render('error.html', {}))

def main():
	app = webapp.WSGIApplication([
			(r'/(.*)', MyHandler)], debug=True)
	wsgiref.handlers.CGIHandler().run(app)

if __name__ == '__main__':
	main()
