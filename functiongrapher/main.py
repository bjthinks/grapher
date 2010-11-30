#!/usr/bin/env python

import wsgiref.handlers
from google.appengine.ext import db
from google.appengine.ext import webapp
from google.appengine.ext.webapp import template

class MyHandler(webapp.RequestHandler):
	def get(self, *groups):
                if groups[0] == 'the_image.svg':
                        self.response.out.write(template.render('the_image.svg', {}))
		elif groups[0] == 'favicon.ico':
			self.response.out.write(template.render('favicon.ico', {}))
                else:
                        values = {}
                        self.response.out.write(
                                template.render('main.html', values))
                        self.response.out.write('<p>You accessed this page at the URL: /' + groups[0] + "</p>")
			self.response.out.write(
				template.render('footer.html', values))

def main():
	app = webapp.WSGIApplication([
	  (r'/(.*)', MyHandler)], debug=True)
	wsgiref.handlers.CGIHandler().run(app)

if __name__ == '__main__':
	main()
