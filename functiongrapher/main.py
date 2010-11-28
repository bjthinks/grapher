#!/usr/bin/env python

import wsgiref.handlers
from google.appengine.ext import db
from google.appengine.ext import webapp
from google.appengine.ext.webapp import template

class Geek(db.Model):
	message = db.StringProperty(required=True)
	when = db.DateTimeProperty(auto_now_add=True)
	who = db.StringProperty()

class MyHandler(webapp.RequestHandler):
	def get(self, *groups):
		geeks = db.GqlQuery('SELECT * FROM Geek '
							'ORDER BY when DESC')
		values = { 'geeks': geeks }
		self.response.out.write(
			template.render('main.html', values))
		self.response.out.write('You accessed this page at the URL: /' + groups[0])
	def post(self, *groups):
		geek = Geek(message=self.request.get('message'),
					who=self.request.get('who'))
		geek.put()
		# self.response.out.write('posted!')
		self.redirect('/')

def main():
	app = webapp.WSGIApplication([
	  (r'/(.*)', MyHandler)], debug=True)
	wsgiref.handlers.CGIHandler().run(app)
	
if __name__ == '__main__':
	main()