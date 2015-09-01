# HTTP Client/Server for ClozureCL

The `http` package is a simple package used for performing HTTP requests and parsing the responses. In addition, it has built-in support for keep-alive requests, and [HTTP server-sent events](http://www.w3.org/TR/2011/WD-eventsource-20111020/).

In addition to being a good HTTP client, it also comes with a bare-bones HTTP server. It will handle parse incoming HTTP requests, hand them off to your handler function, and reply with the response returned by you.

It makes heavy use of many other packages:

* [PARSE](http://github.com/massung/parse)
* [RE](http://github.com/massung/re)
* [LEXER](http://github.com/massung/lexer)
* [URL](http://github.com/massung/url)
* [BASE64](http://github.com/massung/base64)

Other packages that are very useful in addition (but not required) are:

* [MARKUP](http://github.com/massung/markup)
* [HTML](http://github.com/massung/html)
* [RFC-DATE](http://github.com/massung/rfc-date)

For the sake of keeping this README trimmed down (there's a lot to cover), it will be assumed that you are familiar with the [`url`](http://github.com/massung/url) package. It it also assumed that you know what the basic anatomy of an HTTP request and response are.

## The Client Package (`:http`)

It's best to understand what's going on under-the-hood first by using the low-level API. So, first, let's create a simple request:

    CL-USER > (make-instance 'request :url "google.com")
    #<REQUEST GET HTTP "http://google.com">

Let's now perform the request and get a response.

    CL-USER > (http-perform *)
    #<RESPONSE 301 "Moved Permanently">

Hmm... looks like the resource isn't where we thought it would be. Let's get a new request to the correct location, generated from the response.

    CL-USER > (http-follow-request *)
    #<REQUEST GET "http://www.google.com/">

That looks more like it. Now let's try and get that one.

    CL-USER > (http-perform *)
    #<RESPONSE 200 "OK">

Now that we've gotten a valid response, let's see what the content of the response is.

    CL-USER > (resp-body *)
    "<!doctype html><html>...</html>"

The above may seem like a lot of work for a simple request. However, there are a lot of helper functions that wrap most of this up. It's important to understand at each step of the way what's being done so that you can best use the library for your needs.

### Breaking It Down...



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

    CL-USER > (setf s (open-http-stream req))
    #<BASIC-TCP-STREAM ISO-8859-1 (SOCKET/9) #x39298028120D>

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

### Helpful Macros

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

## HTTP Server-Sent Events

Adhearing to [http://www.w3.org/TR/2011/WD-eventsource-20111020/](http://www.w3.org/TR/2011/WD-eventsource-20111020/), you can easily open an event stream to process continuous events sent from an HTTP server.

    (open-http-event-stream url event-callback &key method headers redirect-limit)

The *event-callback* should be a function that takes 3 parameters when called: the event type, an id, and a data value. The id and data are both optional and may be `NIL`.

    CL-USER > (flet ((process-event (type id data)
                       (format t "New event: ~s ~s ~s~%" type id data)))
                (open-http-event-stream "url.com" #'process-events))

## The Server Package (`:http-server`)

If you would like to host a simple HTTP server, this package handles accepting the incoming connections, parsing the requests, and sending your responses back.

    (http-simple-server handler server-config &key name port)

The simplest, "Hello, world" server example would be:

    CL-USER > (http-simple-server #'(lambda (resp)
                                      (http-ok resp "Hello, world!")))
    #<PROCESS HTTP Server>

Let's test it.

    CL-USER > (http-get "localhost:8000")
    #<RESPONSE 200 "OK">

    CL-USER > (resp-body *)
    "Hello, world!"

Each time a request is made, the *handler* function supplied is called with the HTTP response that will be returned back (note: the response can get the request). You are expected to fill out the response appropriately.

### Responses

In the example, you probably noticed the `http-ok` function being used to generate the response. The `:http-server` package comes with a myriad of response generation function. They set the correct response code, status message, headers (if required by the response code), and allow for an optional body.

It's probably easiest to just look at `server.lisp` to get the entire list. But all of the major response codes are there (e.g. `http-created`, `http-not-found`, `http-bad-gateway`, ...).

### Router Functions

The server package comes with a macro for defining router functions.

    (define-http-router router &body routes)

This will create a function (*router*) that will attempt to match the path of the request with all the *routes* defined. If no route matches, then the an automatic `http-not-found` response is sent for you.

Each route should be declared as:

    (method path route-handler-function)

Here is an example router:

    CL-USER > (define-http-router my-router
                (:get  "/"         'homepage)
                (:get  "/user/:id" 'user-page)
                (:post "/login"    'login-page))

Most important in this example is the *user-page* route: it shows the use of a keyword argument to the *route-handler*, which should now be defined as:

    (defun user-page (resp &key id) ...)

If a user were to request the page `my-site.com/user/10383`, when *user-page* is called, *id* will be bound to "10383".

## Generating HTML

If you'd like to generate HTML in your request handler, I suggest you take a look at my [`html`](http://github.com/massung/html) package, which can be used to do so quite efficiently.
