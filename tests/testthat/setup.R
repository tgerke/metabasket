# Setup file for testthat tests
# This runs once before all tests

# Register a foreach backend for packages that use foreach internally.
# The bhmbasket package uses foreach with %dopar% internally, and without
# a registered backend, it prints "Caution" messages to stdout. We register
# doFuture with a sequential plan to suppress these messages while maintaining
# deterministic test behavior. This follows doFuture best practices for
# integrating with packages that use foreach (see doFuture vignette 2).
if (requireNamespace("doFuture", quietly = TRUE) && 
    requireNamespace("foreach", quietly = TRUE)) {
  doFuture::registerDoFuture()
  future::plan(future::sequential)
}
