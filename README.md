# HTTP Client/Server for ClozureCL

The `http` package is a simple package used for performing HTTP requests and parsing the responses. In addition, it has built-in support for keep-alive requests, and [HTTP server-sent events](http://www.w3.org/TR/2011/WD-eventsource-20111020/).

In addition to being a good HTTP client, it also comes with a good HTTP server. It will handle parse incoming HTTP requests, route them, and return responses. The server is multi-threaded, has sessions built-in, and allows for HTTP continuations as well.

The `http` package requiresthe following packages:

* [SHA1](http://github.com/massung/sha1)
* [PARSE](http://github.com/massung/parse)
* [RE](http://github.com/massung/re)
* [LEXER](http://github.com/massung/lexer)
* [URL](http://github.com/massung/url)

Other packages that are very useful in addition (but not required) are:

* [MARKUP](http://github.com/massung/markup)
* [HTML](http://github.com/massung/html)
* [RFC-DATE](http://github.com/massung/rfc-date)

For the sake of keeping this README trimmed down (there's a lot to cover), it will be assumed that you are familiar with the [`url`](http://github.com/massung/url) package. It it also assumed that you know the basic anatomies of HTTP requests and responses.

## Quickstart

Let's start at the lowest level, walk through a simple example, and then jump to the helper APIs...

First, let's make a simple HTTP request:

    CL-USER > (http-make-request "google.com")
    #<REQUEST GET http://google.com/>

Next, let's perform the request, which should get us a response.

    CL-USER > (http-perform *)
    #<RESPONSE 301 "Moved Permanently">

Hmm... looks like the resource isn't where we thought it would be. Let's follow the response to where it says the resource will be.

    CL-USER > (http-follow *)
    #<RESPONSE 200 "OK">

That looks more like it. Now that we've gotten an OK response, let's see what the content of the response is.

    CL-USER > (resp-body *)
    "<!doctype html><html>...</html>"

The above may seem like a lot of work for a simple request. However, there are a lot of helper functions that wrap most of this up. It's important to understand at each step of the way what's being done so that you can best use the library for your needs.

### HTTP Streams

The `http-perform` function takes an optional parameter that is a socket stream. If you don't provide one, then one is created for you (this is the most common use-case). Sometimes, though, you'll want to create your own socket, and keep it alive to make several requests without having to tear it down and open another one up. To do this, you'll need to use the `http-open-stream` function.

    (http-open-stream request &optional timeout)   ;=> stream

Additionally, when you create the request, you'll need to pass `:keep-alive t`, so that the connection isn't closed after the response is read.

    (http-make-request url :keep-alive t)

*NOTE: even if you want to keep the connection alive, the server may respond with a "Connection: close" header, in which case the output side of the socket will be shutdown.*

Be sure and close the sockets when you are done!

### HTTP Headers

Both `request` and `response` objects derive from the same base class: `headers`. The `http-headers` accessor method can be used to both extract and set the headers of a request or response.

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
   ("Connection" "close")
   ...)

The `http-header` method can be used to extract a single header from a request or response.

    CL-USER > (http-header (http-get "google.com") "Content-Type")
    "text/html; charset=ISO-8859-1"

The `http-header` method is also `setf`-able, and can be used to change or add headers to a request or response.

There are many situations in which a header may actually exist many times in a response or request (e.g. "Set-Cookie" headers). For those situations, `http-header` also takes an optional *all* keyword parameter that defaults to nil. When non-nil, it will return a list of all the headers it finds.

Since `request` is also a subclass of `headers`, you can optionally set headers whenever creating the your request.

    CL-USER > (http-make-request "apple.com" :headers '(("x-my-header" 10)))
    #<REQUEST GET http://apple.com>

### Encoded Responses/Requests

Although the response is automatically read for you, the body of the response may not be. If you supply `:read-body nil` to the request (the default is T), then the body will not be read.

By default, the `http` package will send an "Accept-Encoding" header with your requests. However, the only accepted encoding is the "identity" encoding. With only this encoding, a server *shouldn't* send an zipped or encoded responses.

You can, however, register other encodings with the `http` package, and those encodings will automatically be included in the "Accept-Encoding" header with all requests.

    (http-register-content-encoding name &key encoder decoder)

For example, if you had the functions `gzip` and `gunzip`, you could register the "gzip" encoding with those functions, like so...

    (http-register-content-encoding "gzip" :encoder 'gzip :decoder 'gunzip)

...and then all requests made will include "gzip" in the list of accepted encodings to a server.

Additionally, all requests made can have their bodies encoded as well. Simply set the "Content-Encoding" header in your request to the encoding type you'd like to use and the data will be thusly encoded for you before it is sent (the body of the request will not be altered, only the data sent over the wire).

*NOTE: the same is true of the "Content-Type", simply setting it in the header of a request will encoded the body of the request properly for you before sending it. And any decoded response will use the "Content-Type" as well.*

### Wrapper Functions

All of the above is nicely wrapped up for you in the following helper functions:

    (http-get url &rest request-initargs)
    (http-head url &rest request-initargs)
    (http-options url &rest request-initargs)
    (http-trace url &rest request-initargs)
    (http-delete url &rest request-initargs)
    (http-put url &rest request-initargs)
    (http-post url &rest request-initargs)
    (http-patch url &rest request-initargs)

Each will create a request to the URL provided along with the other keyword initargs. The `:read-body` initarg will be `NIL` for `http-head`. The request will be performed and any redirects will be automatically followed for you (up to 3 times).

    CL-USER > (http-get "google.com")
    #<RESPONSE 200 "OK">

The available *request-initargs* are:

**:method** The HTTP verb. Defaults to "GET".

**:protocol** The HTTP protocol. Default is "1.0".

**:headers** An associative list of (header value) pairs. Default is NIL.

**:keep-alive** If T, do not close the socket. Default is NIL.

**:read-body** Reads the respone body if T. Default is T.

**:body** Binary or string data to send with the request. Default is NIL.

### Helpful Macros

The `with-response` macro can be used when you only care about successful requests (i.e. `(<= 200 resp-code 299)` is `T`).

    CL-USER > (with-response (resp (http-get "www.apple.com"))
                (resp-code resp))
    200
    T

This can return up to 3 values. If the request was successful, the return value will be the result of the body and `T` to indicate that the request was successful. If the request failed, the return values will be `NIL` (no result), `NIL` (failure), and the response.

    CL-USER > (with-response (resp (http-get "www.apple.com/foo"))
                (resp-code resp))
    NIL
    NIL
    #<RESPONSE 404 "Not Found">

The `with-headers` macro is useful if you would like to bind variables to header values and also make them `setf`-able within a body of code.

    (let ((req (http-make-request "www.google.com")))
      (with-headers ((dummy "x-my-dummy-header"))
          req
        (setf dummy "My dummy value")))

*NOTE: you can bind variables in `with-headers` to headers that don't exist yet, but will be added after `setf`-ing them.*

## HTTP Server-Sent Events

Adhearing to [http://www.w3.org/TR/2011/WD-eventsource-20111020/](http://www.w3.org/TR/2011/WD-eventsource-20111020/), you can easily open an event stream to process continuous events sent from an HTTP server.

    (http-open-event-stream url event-callback &key method headers redirect-limit)

The *event-callback* should be a function that takes 3 parameters when called: the event type, an id, and a data value. The id and data are both optional and may be `NIL`.

    CL-USER > (flet ((process-event (type id data)
                       (format t "New event: ~s ~s ~s~%" type id data)))
                (http-open-event-stream "url.com" #'process-events))

## Creating an HTTP Server

*Coming soon...*

That's it!
