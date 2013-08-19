# Simple HTTP Interface for LispWorks

The `HTTP` package is a dead-simple package used for performing simple HTTP requests and parsing the responses. It also can parse URLs, escape and unescape strings, and parse query strings.

It makes heavy use of my [`re`](http://github.com/massung/re) and [`lexer`](http://www.github.com/massung/lexer) packages, so to understand some of the code you should start with those. But it really is simple.

## Quickstart

After loading the package, the first step is to parse a URL.

	CL-USER > (parse-url "www.google.com")
	#<URL "http://www.google.com/">
	
Once you have a URL, you can create a request.

	CL-USER > (make-instance 'request :url * :method :get)
	#<REQUEST GET "http://www.google.com/">
	
Next, the `with-http-request` macro opens a TCP socket to the remote server and sends a request. It then allows you to execute a body of code for the response.

	CL-USER > (with-http-request (http *) (read-http-response http))
	#<RESPONSE 200 "OK">
	
Finally, since you know the response was OK, you can finish everything up by reading the body from the socket. The body is read separate from the response, because sometimes you won't care (e.g. a redirect)... and why waste resources?

	CL-USER > (read-http-body http)
	"<!doctype html><html>...</html>"

## Even Quicker?

While that was an overview of all the utility functions and objects to get the contents of a URL, the `http` package also nicely wraps most of them up for you with various `http-` helper functions.

	CL-USER > (http-get "www.google.com")
	200
	"OK"
	"<!doctype html><html>...</html>"
