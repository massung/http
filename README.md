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

While that was an overview of all the utility functions and objects to get the contents of a URL, the `http` package also nicely wraps most of them up for you with various `http-` helper functions:

	;; perform a HEAD request
	(http-head url &key headers follow-redirects) ;=> response

	;; perform a GET request
	(http-get url &key headers) ;=> response body
	
	;; perform a PUT request
	(http-put url data &key headers) ;=> response
	
	;; perform a POST request
	(http-post url data &key headers) ;=> response

	;; perform a DELETE request
	(http-delete url &key headers) ;=> response

The optional `headers` should be an associative list of key/value pairs that will be sent with the request. The `http` package will already take care of any obvious headers for you (e.g. Host, Connection, and Authorization).

For GET requests, `follow-redirects` signals that if the resource has moved (response codes in the 300's). If it is a positive integer, then only that many follows will occur. If it is `t` then it will keep following the resource until it finds it or fails.

Both PUT and POST requests have a `data` argument, which is what is sent in the body of the request. *At this time there is no support for multi-part posts*.

	CL-USER > (http-get "google.com" :follow-redirects t)
	#<RESPONSE 200 "OK">
	"<!doctype html><html>...</html>"

If the `response-code` returned is not a success code (200-299) then the body is not read.