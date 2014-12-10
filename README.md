# HTTP Package for LispWorks

The `http` package is a dead-simple package used for performing HTTP requests and parsing the responses. In addition, it has built-in support for keep-alive requests, and [HTTP server-sent events](http://www.w3.org/TR/2011/WD-eventsource-20111020/).

In addition to being a good HTTP client, it also comes with a bare-bones HTTP server. It will handle parse incominb HTTP requests, hand them off to your handler function, and reply with the response returned by you.

It makes heavy use of my [`re`](http://github.com/massung/re) and [`lexer`](http://www.github.com/massung/lexer) packages, so to understand some of the code you should start with those. But it really is simple. It also uses my [`base64`](http://github.com/massung/base64) package.

## The Client Package (`:http`)

It's best to understand what's going on under-the-hood first by using the low-level API. First, let's parse a URL we want to request:

	CL-USER > (parse-url "google.com")
	#<URL HTTP "http://google.com">
	
Next, let's create a request for that URL:

	CL-USER > (make-instance 'request :url *)
	#<REQUEST GET HTTP "http://google.com">
	
Let's now perform the request and get a response.

	CL-USER > (http-perform *)
	#<RESPONSE 301 "Moved Permanently">
	
Hmm... looks like the resource isn't where we thought it would be. Let's generate a new request from the response.

	CL-USER > (http-follow-request *)
	#<REQUEST GET "http://www.google.com/">

That looks more like it. Now let's try and get that one.

	CL-USER > (http-perform *)
	#<RESPONSE 200 "OK">

Now that we've gotten a valid response, let's see what the content of the response is.

	CL-USER > (response-body *)
	"<!doctype html><html>...</html>"

The above may seem like a lot of work for a simple request. However, there are a lot of helper functions that wrap most of this up. It's important to understand at each step of the way what's being done so that you can best use the library for your needs.

### Constructing URLs

URLs can be constructed in one of three ways:

1. By hand with `make-instance`
2. Parsing a string with `parse-url`
3. Copying an existing URL with `copy-url`

Each URL - if constructed with `make-instance` has the following initargs: `:scheme`, `:domain`, `:port`, `:auth`, `:path`, `:query`, and `:fragment`.

All of these members of a URL have valid defaults except for the domain, which must be supplied. The scheme should be either `:http` or `:https`. The auth should be a list of "username" and "password".
	
Parsing URLs is done with `parse-url`, but in addition to parsing a string, it also allows for overriding any of the initargs that are parsed.

	CL-USER > (parse-url "google.com" :port 8000)
	#<URL "http://google.com:8000/">

Once you have a URL, you can use `copy-url` to create a new URL, and - like `parse-url` - override any of the initargs.

	CL-USER > (copy-url * :query '(("foo" "bar")))
	#<URL "http://google.com:8000/?foo=bar">
	
Wrapping up both `parse-url` and `copy-url` nicely is the `with-url` macro.

	(with-url ((url url-form &rest initargs) &body body)

The `url-form` can be either another URL object or a string.

	CL-USER > (with-url (url "apple.com" :path "/trailers") url)
	#<URL "http://apple.com/trailers">

	CL-USER > (with-url (url * :scheme :https :auth '("my" "password")) url)
	#<URL "https://my:password@apple.com/trailers">

### The Anatomy of a Request

### The Anatomy of a Response

### HTTP Headers

Both `request` and `response` objects derive from the same base class which contains headers. The `http-headers` accessor method can be used to both extract and set the headers of a request or response.

	CL-USER > (http-headers (http-get "www.google.com"))
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

### Opening HTTP Streams

If you'd like to create your own HTTP socket stream to issue (one or more) requests on, use the `open-http-stream` function, and pass a URL object to it.

	(open-http-stream url &key keep-alive errorp timeout)

This will automatically choose the correct port for the scheme (and URL), perform TLS (if a secure scheme), etc.

This socket will remain open as long as the server doesn't close it and any requests performed on it are set to `:keep-alive t`.

### Performing Requests

As seen above, the method of issuing requests and parsing responses is with `http-perform`:

	(http-perform req &optional stream)
	
If you do not provide an HTTP socket stream, then one will be created for you (and returned as part of the response). After the response is done being parsed, if the request didn't ask for the socket to remain open it will be closed for you.

This is the best method of performing multiple requests to the same server without closing and re-opening sockets multiple times.

	CL-USER > (setf req (make-instance 'request :url "www.google.com" :keep-alive t))
	#<REQUEST GET "http://www.google.com/">
	
	CL-USER > (setf s (open-http-stream "www.google.com"))
	#<COMM:SOCKET-STREAM 227ECE83>
	
	CL-USER > (loop for i below 3 collect (http-perform req s))
	(#<RESPONSE 200 "OK">
	 #<RESPONSE 200 "OK">
	 #<RESPONSE 200 "OK">)
	
	CL-USER > (close s)
	T

*REMEMBER: You are responsible for closing your own streams!*

### Following Redirects

If the response you get back is telling you that the resource has been moved, the `http-follow-request` method can be used to get a new request. The new request will maintain the same method, headers, keep-alive, data, and read-body (except when the `response-code` was a 303 indicating that the new method should be "GET").

	(http-follow-request resp)

There is also the `http-follow` function, which will get the new request and perform it for you.

	(http-follow resp &key redirect-limit)
	
The default *redirect-limit* is 3.

### Wrapping It All Up!

All of the above is nicely wrapped up for you in the following helper functions:

	(http-head url &key headers keep-alive redirect-limit)
	(http-get url &key headers keep-alive redirect-limit)
	(http-options url &key headers keep-alive redirect-limit)
	(http-trace url &key headers keep-alive redirect-limit)
	(http-delete url &key headers keep-alive redirect-limit)
	(http-put url &key headers data keep-alive redirect-limit)
	(http-post url &key headers data keep-alive redirect-limit)
	(http-patch url &key headers data keep-alive redirect-limit)

Each will create a request to the URL provided along with the other arguments. And the `:read-body` initarg will be `T` in all cases except for `http-head`. The request will be performed and any redirects will be automatically followed for you.

	CL-USER > (http-get "google.com")
	#<RESPONSE 200 "OK">

That's it!

*NOTE: As with `http-follow`, the default `:redirect-limit` is set to 3.*

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

### Additional Utility Functions

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

## Using HTTP Server-Sent Event

Adhearing to [http://www.w3.org/TR/2011/WD-eventsource-20111020/](http://www.w3.org/TR/2011/WD-eventsource-20111020/), you can easily open an event stream to process continuous events sent from an HTTP server.

	(open-http-event-stream url event-callback &key method headers redirect-limit)

The *method* defaults to "GET" and the *redirect-limit* defaults to 3.

The *event-callback* should be a function that takes 3 parameters when called: the event type, an id, and a data value. The id and data are both optional and may be `NIL`.

	CL-USER > (flet ((process-event (type id data)
	                   (format t "~s ~s ~s~%" type id data)))
	            (open-http-event-stream "url.com" #'process-events))

## The Server Package (`:http-server`)

If you would like to host a simple HTTP server, this package handles accepting the incoming connections, parsing the requests, and sending your responses back.

	(simple-http router server-config &key name port)

The simplest, "Hello, world" server example would be:

	CL-USER > (simple-http #'(lambda (req server-config) (http-ok server-config)) "Hello, world!")
	#<MP:PROCESS Name "HTTP Simple Server on port 8000" Priority 3 State "Running">
	
Let's test it.
	
	CL-USER > (http-get "localhost:8000")
	#<RESPONSE 200 "OK">

	CL-USER > (response-body *)
	"Hello, world!"

Each time a request is made, the *route* function supplied is called with the HTTP request and the *server-config*. You are expected to create and return a `response` object, which is then sent back.

## Response Generation

In the example, you probably noticed the `http-ok` function being used to generate the response. The `:http-server` package comes with a myriad of response generation function. They set the correct response code, status message, headers (if required by the response code), and allow for an optional body.

It's probably easiest to just look at `server.lisp` to get the entire list. But all of the major response codes are there (e.g. `http-created`, `http-not-found`, `http-bad-gateway`, ...).

While for simple cases you'll just be creating and returning the response at the same time, you can also create a response, modify it (e.g. set headers), and then return it.

## Generating HTML

If you'd like to generate HTML in your request handler, I suggest you take a look at my [`html`](http://github.com/massung/html) package, which can be used to do so quite efficiently.
