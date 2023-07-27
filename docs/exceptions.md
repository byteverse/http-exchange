# Exceptions and Resources

This library does not throw unchecked exceptions, and it is designed to
work with backends that do not throw unchecked exceptions. See the
`network-unexceptional` library for an example of functions compatible
with the `network` library's types that return error codes in an `Either`
instead of throwing them.  

Since this library is flexible in the monadic context in which HTTP requests
are performed, unchecked exceptions are not even an option. This is because
they only work with `IO`, and the test suite uses a non-IO monadic context.
We end up with several options for how to deal with exceptions and with
resources (sockets). The signature for `send` could be:

    (A) send :: Chunks -> M ()
    (B) send :: Chunks -> M (Either TransportException ())
    (C) send :: Resource -> Chunks -> M ()
    (D) send :: Resource -> Chunks -> M (Either TransportException ())

Options A and D are opposite ends of a spectrum. Option D says "we prefer
explicitly handling resources and errors instead of letting the monadic
context handle them", and option A says "push everything into the monadic
context". To deal with the resource, the monadic context needs to be
reader-like, and to deal with the exception, the monadic context needs
to be except-like (see `ExceptT` from `transformers`). Notice that
option D requires the indefinite module's signature to include additional
types: `Resource` and `TransportException`.

All of these options are, in a sense, equivalent. None of them is able
to express anything that the others cannot. Option A subsumes them all
because we can just instantiate `M` with transformer stack including
`ExceptT TransportException` and `ReaderT Resource`. From option D,
we can recover option A by assigning `Resource = ()` and
`TransportException = Void`.

Intuition suggests picking the simplest thing (option A), and this library
originally leaned in that direction (the 0.1.0.0 release used option C).
However, in practice, the ergonomics of option D are nicer. This is,
like any assessment of ergonomics, a subjective call, but here are
three ways in which option D provides a better experience to end users:

1. Avoiding nested `Either`. With option A or C, we end up with something
   like this as the concrete result type of our `exchange` function:
   `ExceptT TransportException IO (Either HttpException Response)`. With
   option A, we can instead define a sum type, which is currently what
   we do, to pull the two possible types of exceptions into a single
   type. Then, our result type becomes (using a hypothetical anonymous
   sums syntax): `IO (Either (TransportException | HttpException) Response)`.
   A nested `Either` makes it more likely that a user accidentally ignores
   an exception.
2. The result type of `exchange`, as mentioned above, is just in `IO` and
   doesn't require the user to call `runExceptT` or `runReaderT`.
3. The signatures of `send` and `receive` are closer to what most existing
   implementation actually use. Use of `ExceptT` and `ReaderT` is rare
   in functions exposed by networking libraries.

Of these three ergonomic improvements, the author has found improvement 1
to be the more significant.
