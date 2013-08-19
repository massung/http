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
	
Next, the `http-perform` function will issue the request to the server and return the response.

	CL-USER > (http-perform * :read-body t)
	#<RESPONSE 200 "OK">
	
The body was optionally read, because sometimes you won't care (e.g. a redirect)... and why waste resources?

	CL-USER > (response-body *)
	"<!doctype html><html>...</html>"

## Even Quicker?

While that was an overview of all the utility functions and objects to get the contents of a URL, the `http` package also nicely wraps most of them up for you with various helper functions:

	;; perform a HEAD request
	(http-head url &key headers follow-redirect)

	;; perform a GET request
	(http-get url &key headers follow-redirect)
	
	;; perform a PUT request
	(http-put url &key data headers)
	
	;; perform a POST request
	(http-post url &key data headers)

	;; perform a DELETE request
	(http-delete url &key headers)

All the `http-` helper functions use the `with-url` macro, which allows you to pass either a `url` object or a string, which will be parsed on-demand.

	(with-url ((url url-expr) &body body)

The optional `headers` should be an associative list of key/value pairs that will be sent with the request. The `http` package will already take care of any obvious headers for you (e.g. Host, Connection, and Authorization).

For HEAD and GET requests, `follow-redirect` signals that if the resource has moved (response codes in the 300's) that an additional `http-perform` should be taken to follow the resource to its new location. If the resource has continued to move, it will be up to you to test and follow.

	CL-USER > (http-get "google.com" :follow-redirect t)
	#<RESPONSE 200 "OK">

Both PUT and POST requests have an optional `data` argument, which is what is sent in the body of the request. *At this time there is no support for multi-part posts*.

	CL-USER > (http-post "httpbin.org/post" :data "Echo")
	#<RESPONSE 200 "OK">

The `http-get`, `http-put`, and `http-post` functions will all send `:read-body t` to `http-perform`. The body is only read if the response code is a success (200-299).

## Additional Utilities

TODO: url-encode, url-decode, base64-encode, base64-decode, ...