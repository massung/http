# HTTP Package for LispWorks

The `http` package is a dead-simple package used for performing simple HTTP requests and parsing the responses. It also can parse URLs, escape and unescape strings, and parse query strings.

In addition to being a good HTTP client, it also comes with a bare-bones HTTP server. It will handle parse incominb HTTP requests, hand them off to your handler function, and reply with the response returned by you.

It makes heavy use of my [`re`](http://github.com/massung/re) and [`lexer`](http://www.github.com/massung/lexer) packages, so to understand some of the code you should start with those. But it really is simple. It also uses my [`base64`](http://github.com/massung/base64) package.

## The Client Package (`:http`)

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

Following a redirect is simply a matter of fetching the "Location" header in the response, creating a new request with the new URL, and then calling `http-perform` with the new request. The `http-follow` function takes a response object (and an optional `:redirect-limit` for the number of hops to take) and does just that.

	CL-USER > (http-get "google.com")
	#<RESPONSE 301 "Moved Permanently">
	
	CL-USER > (http-follow * :redirect-limit 1)
	#<RESPONSE 200 "OK">

*NOTE: If the response doesn't contain a new "Location" to follow, an error is signaled.*

## Parsing Headers

Both `request` and `response` objects derive from the same base class which contains headers. The `http-headers` accessor method can be used to both extract and set the headers of a request or response.

	CL-USER > (http-headers (http-get "google.com"))
	(("Location" "http://www.google.com/")
	 ("Content-Type" "text/html; charset=UTF-8")
	 ("Date" "Thu, 10 Apr 2014 18:08:21 GMT")
	 ("Expires" "Sat, 10 May 2014 18:08:21 GMT")
	 ("Cache-Control" "public, max-age=2592000")
	 ("Server" "gws")
	 ("Content-Length" "219")
	 ("X-XSS-Protection" "1; mode=block")
	 ("X-Frame-Options" "SAMEORIGIN")
	 ("Alternate-Protocol" "80:quic")
	 ("Connection" "close"))

There is the method `http-header` that can be used to extract a single header from a request or response.

	CL-USER (http-header (http-get "google.com") "Location") 
	"http://www.google.com/"

The `http-header` method is also `setf`-able, and can be used to change or add headers to a request or response.

Finally, the `with-headers` macro is useful if you would like to bind variables to header values and also make them `setf`-able within a body of code.

	CL-USER > (with-headers ((dummy "x-my-dummy-header"))
	              (http-get "google.com")
	            (setf dummy "My dummy value"))
	"My dummy value"

## Handling Encoded Content

By default, the `http` package won't send an "Accept-Encoding" header with your request. Without this, a server shouldn't send an encoded body. You can, however, send the header yourself, and then decode the body after you have received a response.

	CL-USER > (http-get "httpbin.org/gzip" :headers '(("Accept-Encoding" "gzip")))
	#<RESPONSE 200 OK>

Now, assuming you have a function `gzip:unzip`, you can decode the body with something like this:

	CL-USER > (let* ((encoding (http-header * "Content-Encoding"))
	                 (decoder (cond
	                           ((string= encoding "identity") #'identity)
	                           ((string= encoding "gzip") #'gzip:unzip)
	                           (t
	                            (error "Unknown Content-Encoding: ~a" encoding)))))
	            (funcall decoder (response-body *))))

*NOTE: At some point the "gzip" encoding will be added to the `http` package and this will be done for you.*

## Constructing URLs

While the `url` class is exported, and you are certainly free to `(make-instance 'url ...)`, the `with-url` macro can be extremely helpful.

	(with-url ((url url-form &rest initargs) &body body)

The `url-form` can be either another `url` object or a string to be parsed with `parse-url`. After `url-form` is evaluated, if you passed any `initargs`, they will be used to override the various initargs before being bound to the `url` variable.

	CL-USER > (with-url (url "apple.com" :path "/trailers") url)
	#<URL "http://apple.com/trailers">

	CL-USER > (with-url (url * :scheme :https :auth '("my" "password")) url)
	#<URL "https://my:password@apple.com/trailers">

This is extremely handy.

*NOTE: The `parse-url` function also takes these `initargs` as well, allowing you to override whatever was parsed!*

## Even Quicker?

While that was an overview of what's going on under-the-hood, those three steps are wrapped up nicely in the following helper functions:

	(http-head url &key headers redirect-limit)
	(http-get url &key headers redirect-limit)
	(http-delete url &key headers redirect-limit)
	(http-put url &key headers data redirect-limit)
	(http-post url &key headers data redirect-limit)
	(http-patch url &key headers data redirect-limit)

Each of these functions use the `with-url` macro, allowing you to pass either a `url` object or a string.

The optional `headers` should be an associative list of key/value strings that will be sent with the request. 

*NOTE: `http-perform` already takes care of any obvious headers that will need to be sent: Host, Connection, Content-Length, and Authorization.*

PUT, POST, and PATCH requests have an optional `data` argument, which is what is sent in the body of the request. *At this time there is no support for multi-part posts*.

The `redirect-limit` defaults to 3. 

## Additional Utility Functions

The `http` package also comes with quite a few helper functions to assist you in generating requests and handling responses.

The `encode-url` function takes a string and makes sure that all characters are properly encoded for the query string of a URL.

	CL-USER > (encode-url "$9.50+/-5 cents")
	"%249.50%2B%2F-5%20cents"

The inverse is `decode-url`, which given an encoded string will decode it into the original value.

	CL-USER > (decode-url *)
	"$9.50+/-5 cents"
	
The `make-query-string` function takes an list of key/value pairs and generates a url-encoded query string. This function is used internally for all URL query strings, but you can use it for PUT and POST request bodies.

	CL-USER > (make-query-string '(("q" "henry gale") ("limit" 10)))
	"q=henry+gale&limit=10"

The inverse of `make-query-string` is `parse-query-string`. Given a query string, it will parse it into a list of key/value pairs.

	CL-USER > (parse-query-string *)
	(("q" "henry gale") ("limit" "10"))
	
*NOTE: While `make-query-string` takes any value and converts it to a string, `parse-query-string` will only return strings for values as it is unaware of the type. It's up to you to parse the value appropriately.*
	
The `with-response` macro can be used when you only care about successful requests (i.e. `(<= 200 response-code 299)` is `T`).

	CL-USER > (with-response (resp (http-get "www.apple.com"))
	            (response-code resp))
	200
	T

This can return up to 3 values. If the request was successful, the return value will be the result of the body and `T` to indicate that the request was successful. If the request failed, the return values will be `NIL` (no result), `NIL` (failure), and the response.

	CL-USER > (with-response (resp (http-get "www.apple.com/foo"))
	            (response-code resp))
	NIL
	NIL
	#<RESPONSE 404 "Not Found">

## The Server Package (`:http-server`)

If you would like to host a simple HTTP server, this package handles accepting the incoming connections, parsing the requests, and sending your responses back.

The simplest server you can create would be...

	CL-USER > (simple-http :port 8000)
	#<MP:PROCESS Name "HTTP Simple Server on port 8000" Priority 3 State "Running">
	
Let's test it.
	
	CL-USER > (http-get "localhost:8000")
	#<RESPONSE 404 "Not Found">

We got a 404 back because we haven't defined any routes for the server to test against. Routes are created with the `define-http-route` macro.

	CL-USER > (define-http-route index ()
	              ()
	            (http-ok "Hello, world!"))
	INDEX

Now let's test our new route.

	CL-USER > (http-get "localhost:8000")
	#<RESPONSE 200 "OK">

	CL-USER > (response-body *)
	"Hello, world!"

The `define-http-route` macro is broken up into 3 main parts: the path-spec, route guards, and body. The path-spec is the path in the request to the server. Each element in the path-spec can be a string, a symbol, or a list. 

The following are all examples of path-specs:

	;; /index.html
	("index.html")

	;; /hello/:target
	("hello" target)

	;; /forum/:forum-id/thread/:thread-id
	("forum" forum-id "thread" thread-id)

	;; /static/...
	("static" &rest path)

Route guards are additional tests that are performed and must return `T` for the route body to be executed. These tests are predefined and are the keyword arguments to [`match-route`](https://github.com/massung/http/blob/master/server.lisp#L274).

	;; example guards
	(:methods '(:put :post))

Let's modify the path-spec in our route to say hello to many people.

	CL-USER > (define-http-route index ("hello" &rest names)
	              (:methods '("GET"))
	            (http-ok (format nil "Hello to ~{~:(~a~)~^, ~}" names)))
	INDEX

Let's test it by pointing your browser to [localhost:8000/hello/jeff/tom/dan](http://localhost:8000/hello/jeff/tom/dan).

## Response Generation

In the example, you probably noticed the `http-ok` function being used to generate the response. The `:http-server` package comes with a myriad of response generation function. They set the correct response code, status message, and allow for an optional body. 

It's probably easiest to just look at `server.lisp` to get the entire list. But all of the major response codes are there (e.g. `http-created`, `http-not-found`, `http-bad-gateway`, ...).

While for simple cases you'll just be creating and returning the response at the same time, you can also create a response, modify it (e.g. set headers), and then return it.

## Generating HTML

If you'd like to generate HTML in your request handler, I suggest you take a look at my [`html`](http://github.com/massung/html) package, which can be used to do so quite efficiently.
