## TODO: this would be more useful perhaps if it took an expression
## but getting that right without the restarting delayed promise
## evaluation is hard.
retry <- function(f, delay=0.01, max_delay=0.1, timeout=10) {
  f <- match.fun(f)
  t_max <- Sys.time() + as.difftime(timeout, units="secs")
  i <- 1L
  repeat {
    res <- tryCatch(return(f()),
                    RetryAgain=function(e) NULL)
    if (Sys.time() > t_max) {
      stop(RetryFailed(i))
    }
    Sys.sleep(min(max_delay, i * delay))
    i <- i + 1L
  }
}

retry_logical <- function(f, ...) {
  f <- match.fun(f)
  g <- function() {
    if (f()) TRUE else stop(RetryAgain())
  }
  retry(g, ...)
}

RetryAgain <- function() {
  set_class(list(message="try again", call=NULL),
            c("RetryAgain", "error", "condition"))
}
RetryFailed <- function(n) {
  set_class(list(message=sprintf("Not returned in time after %d attempts", n),
                 call=NULL),
            c("RetryFailed", "error", "condition"))
}

set_class <- function(x, cl, add=FALSE) {
  if (add) {
    class(x) <- c(cl, class(x))
  } else {
    class(x) <- cl
  }
  x
}

file_remove <- function(filename) {
  file.exists(filename) && file.remove(filename)
}

rand_str <- function(n) {
  paste(sample(letters, n, replace=TRUE), collapse="")
}
