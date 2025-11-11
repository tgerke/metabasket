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

# Pre-check which optional packages can be loaded in test environment
# We need to check this BEFORE tests run because some packages may segfault
# when loaded from within tests even if they load fine standalone
.pkg_available <- new.env(parent = emptyenv())

# Check bmabasket
.pkg_available$bmabasket <- tryCatch({
  system.file(package = "bmabasket") != "" && 
    requireNamespace("bmabasket", quietly = TRUE)
}, error = function(e) FALSE)

# Check basket  
.pkg_available$basket <- tryCatch({
  system.file(package = "basket") != "" && 
    requireNamespace("basket", quietly = TRUE)
}, error = function(e) FALSE)

# Check bhmbasket
.pkg_available$bhmbasket <- tryCatch({
  system.file(package = "bhmbasket") != "" && 
    requireNamespace("bhmbasket", quietly = TRUE)
}, error = function(e) FALSE)

# Check clinfun
.pkg_available$clinfun <- tryCatch({
  system.file(package = "clinfun") != "" && 
    requireNamespace("clinfun", quietly = TRUE)
}, error = function(e) FALSE)

# Custom skip function that uses pre-checked package availability
skip_if_pkg_not_available <- function(pkg) {
  is_available <- .pkg_available[[pkg]]
  if (is.null(is_available)) {
    # Package not in our pre-check list, do a simple check
    is_available <- system.file(package = pkg) != ""
  }
  
  if (!is_available) {
    testthat::skip(sprintf("Package '%s' is not available", pkg))
  }
}
