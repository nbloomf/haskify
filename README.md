# Haskify: a Haskell client library for the Shopify REST API

This library provides a small, type-safe, monadic interface for the Shopify API, allowing us to write Haskell programs which query or edit shops. The basic monad, called ``HaskifyM``, hides away the details of HTTP requests and JSON juggling so we can pretend that the API endpoints are ordinary functions.



## Basics

Every kind of object (product, customer, etc) the API might return is represented by a haskell type. The fields of an object are also typed, as are the options which an endpoint might take. These types are under ``src/Haskify/Data/``, ``src/Haskify/Value/``, and ``src/Haskify/Opt/``, respectively. This code is boring and repetitive, and is generated by a few scripts in ``gen/`` using several data files in ``schema/``. Warning: the code generating scripts mostly use awk and sed and are generally terrible; these need to be cleaned up. But generating the library code has some advantages: large-scale edits are very easy, we get around Haskell's record syntax with typeclasses, and future API changes can be handled "easily".

Endpoints provided by the API are represented by monadic Haskell functions. For example, the endpoint GET /admin/products.json is represented by the function

    getProducts :: (GetProductsOpt -> GetProductsOpt) -> HaskifyM [Product]

The function-type parameter is how we specify arguments to the URL. Every endpoint that can take arguments takes a *typed* argument, to ensure that we don't accidentally send an HTTP request with nonsense arguments.

Values in ``HaskifyM`` are called *scripts* and can be executed by supplying the path to a credentials file containing the shop URL and an API key.

Finally, all HTTP requests are logged and when a script is run it generates a "result" and a log separately. This is useful for debugging.



## Examples

Some small utility programs are included as examples.

* ``shopify-post-product``: takes a list of files, each containing a single JSON-encoded product, and posts them to a given shop.