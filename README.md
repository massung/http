# Simple HTTP Interface for LispWorks

The `HTTP` package is a dead-simple package used for performing simple HTTP requests and parsing the responses. It also can parse URLs, escape and unescape strings, and parse query strings.

It makes heavy use of my [`re`](http://github.com/massung/re) and [`lexer`](http://www.github.com/massung/lexer) packages, so to understand some of the code you should start with those. But it really is simple. It also uses my [`base64`](http://github.com/massung/base64) package.

## Quickstart

After loading the package, the first step is to parse a URL.

	CL-USER > (parse-url "www.google.com")
	#<URL HTTP "www.google.com" "/">
	
Once you have a URL, you can create a `request` object.

	CL-USER > (make-instance 'request :url *)
	#<REQUEST GET HTTP "www.google.com" "/">
	
Finally, `http-perform` will open a socket, issue the request to the server, and return a `response` object.

	CL-USER > (http-perform *)
	#<RESPONSE 200 "OK">

	CL-USER > (response-body *)
	"<!doctype html><html>...</html>"

## Following Redirects

Following a redirect is simply a matter of fetching the "Location" header in the response, creating a new request with the new URL, and then calling `http-perform` with the new request. The `http-follow` function takes a response object and does just that.

	CL-USER > (http-get "google.com")
	#<RESPONSE 301 "Moved Permanently">
	
	CL-USER > (http-follow *)
	#<RESPONSE 200 "OK">

*NOTE: If the response doesn't contain a new "Location" to follow, `nil` is returned.*

## Even Quicker?

While that was an overview of what's going on under-the-hood, those three steps are wrapped up nicely in the following helper functions:

	(http-head url &key headers)
	(http-get url &key headers)
	(http-delete url &key headers)
	(http-put url &key headers data)
	(http-post url &key headers data)
	(http-patch url &key headers data)

Each of these functions use the `with-url` macro, allowing you to pass either a `url` object or a string, which will be parsed.

	(with-url ((url url-expr) &body body)

The optional `headers` should be an associative list of key/value strings that will be sent with the request. 

*NOTE: `http-perform` already takes care of any obvious headers that will need to be sent: Host, Connection, Content-Length, and Authorization.*

PUT, POST, and PATCH requests have an optional `data` argument, which is what is sent in the body of the request. *At this time there is no support for multi-part posts*.

## Additional Utility Functions

TODO:

* `make-query-string`
* `parse-query-string`
* `encode-url`
* `with-header`