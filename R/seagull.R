##' Evaluate an expression after acquiring, and while holding, a file
##' lock.  The \code{with_flock_} version uses standard evaluation and
##' is suitable for programming.
##'
##' The behaviour on error is controlled by the \code{error} argument.
##' If \code{TRUE} (the default) then if a lock cannot be established
##' then \code{with_flock} will throw an error and not return.  If
##' there is no error the return value is whatever \code{expr}
##' evaluates to.  (If \code{expr} itself throws an error the lock
##' will always be cleaned up, though this may fail if the lockfile is
##' removed by the code in \code{expr} or another process -- try to
##' avoid this!)
##'
##' If \code{error=FALSE} the return value is always a list of length
##' 2.  The first element is a logical scalar \code{TRUE} or
##' \code{FALSE} indicating if the lock was acquired and the
##' expression evaluated.  The second element is the value of
##' \code{expr} if the lock was acquired or a condition object
##' describing why the lock was not acquired.  If \code{expr} throws
##' an error, that error will still not be caught (use
##' \code{tryCatch}).
##'
##' In either case, if a lock cannot be established the code in
##' \code{expr} is not evaluated.
##'
##' @section Warning:
##'
##' It is not safe to use the file for anything, including locking it
##'   a second time (e.g. \code{with_flock(filename,
##'   with_flock(filename, ...))}).  Simply opening or closing a
##'   handle to a file will break the lock on non-Windows systems
##'   (this is a limitation of the underlying system calls).
##'
##' @title Evaluate expression with file lock
##'
##' @param filename The name of the lockfile.  If \code{NULL}, no
##'   lockfile is used and the action always runs.
##'
##' @param expr Expression to evaluate once the lock is acquired.
##'   This expression must not affect the file \code{filename} in any
##'   way (see warnings in the package README).
##'
##' @param envir Environment in which to evaluate \code{expr}.  The
##'   default is usually reasonable.
##'
##' @param delay Initial period to poll the file for release if it is
##'   locked.  Note this is a \emph{minimum} time to delay.  On POSIX
##'   system with \code{fcntl} I see delays around the 0.2s mark when
##'   accessing files over SMB so small values there are likely
##'   aspirational.  This time is also \emph{additional} to the
##'   \code{fcntl} call (i.e., the pattern is try to lock, then sleep).
##'
##' @param max_delay Maximum period between polls; the delay will grow
##'   from \code{delay} to \code{max_delay} over subsequent calls.
##'
##' @param timeout Total maximum time to wait.  If a lock cannot be
##'   acquired in this period, we either error or return without
##'   evaluating \code{expr} (see Details).
##'
##' @param error Is failure to acquire a lock an error?  If
##'   \code{TRUE} then an error is thrown or the value if \code{expr}
##'   is returned.  If \code{FALSE} the return value is a list with
##'   the first element indicating success or not and the second
##'   element being either a condition or the return value.  See
##'   Details.
##'
##' @param verbose Print information as at each lock acquisition
##'   attempt.  May be useful in debugging.
##'
##' @export
with_flock <- function(filename, expr, envir=parent.frame(),
                       delay=0.01, max_delay=0.1, timeout=Inf, error=TRUE,
                       verbose=FALSE) {
  with_flock_(filename, substitute(expr), envir,
              delay, max_delay, timeout, error, verbose)
}

##' @export
##' @rdname with_flock
with_flock_ <- function(filename, expr, envir=parent.frame(),
                        delay=0.01, max_delay=0.1, timeout=Inf, error=TRUE,
                        verbose=FALSE) {
  fl <- flock(filename)
  ok <- fl$acquire(delay, max_delay, timeout, error, verbose)
  if (fl$acquired) {
    on.exit(fl$release())
    res <- eval(expr, envir)
    if (!error) {
      res <- list(TRUE, res)
    }
  } else {
    res <- ok
  }
  res
}

##' Low-level flock object.  Use this if you need more flexibility
##' than \code{\link{with_flock}}, but understand that if you get it
##' wrong you can cause deadlocks.
##'
##' @title Low-level flock object
##'
##' @param filename Name of file to lock.  \code{NULL} is a fake lock;
##'   acquire always succeeds.
##'
##' @param method Method to use
##' @export
flock <- function(filename, method="fcntl") {
  .R6_flock$new(filename, method)
}

##' @importFrom R6 R6Class
.R6_flock <- R6::R6Class(
  "flock",

  public=list(
    filename=NULL,
    handle=NULL,
    acquired=FALSE,
    lock=NULL,
    method=NULL,

    initialize=function(filename, method) {
      self$filename <- filename
      self$method <- match.arg(method, c("fcntl", "hack"))
      if (is.null(filename)) {
        self$lock <- function(...) TRUE
      } else if (self$method == "fcntl") {
        self$lock <- fcntl_lock
      } else {
        self$lock <- hack_lock
      }
    },

    acquire=function(delay=0.01, max_delay=0.1, timeout=Inf, error=TRUE,
                     verbose=FALSE) {
      if (delay < 0) {
        stop("Delay must be at least zero")
      }
      if (timeout < 0) {
        stop("Timeout must be at least zero")
      }
      if (delay >= max_delay) {
        max_delay <- delay
      }

      if (self$acquired) {
        return(if (error) TRUE else list(TRUE, NULL))
      }

      if (verbose && !is.null(self$filename)) {
        message(sprintf("Acquiring lock on '%s'", self$filename), appendLF=FALSE)
      }
      if (is.null(self$filename)) {
        ## This will return in the self$lock stage.
        ## pass
      } else if (self$method == "fcntl") {
        self$handle <- seagull_open(self$filename)
      } else {
        self$handle <- list(filename=self$filename, NULL)
      }
      res <- tryCatch(
        retry_logical(function() self$lock(self$handle, TRUE),
                      delay, max_delay, timeout, verbose),
        RetryFailed=function(e) set_class(e, "LockFailed", TRUE))

      self$acquired <- isTRUE(res)
      if (error) {
        if (self$acquired) {
          ret <- self$acquired
        } else {
          stop(res)
        }
      } else {
        ## TODO: throw for all non 'LockFailed' errors perhaps?
        ret <- list(self$acquired, if (self$acquired) NULL else res)
      }
      ret
    },

    release=function() {
      ## TODO: Behaviour here is unclear -- on error with error=FALSE,
      ## I think that the lockfile should be closed and marked as
      ## unaquired?
      if (!self$acquired) {
        stop("Cannot release a lock that has not been acquired")
      }
      if (self$lock(self$handle, FALSE)) {
        if (is.null(self$filename)) {
          ## pass
        } else if (self$method == "fcntl") {
          seagull_close(self$handle)
        }
        self$handle <- NULL
        self$acquired <- FALSE
      } else {
        stop("Error closing acquired lock")
      }
      invisible(TRUE)
    }
  ))

seagull_open <- function(filename) {
  list(filename=filename,
       fd=.Call("seagull_open", filename, PACKAGE="seagull"))
}
seagull_close <- function(handle) {
  res <- .Call("seagull_close", handle$fd, TRUE, PACKAGE="seagull")
  ok <- res[[1L]]
  if (!ok) {
    stop("Error closing acquired lock: ", res[[3L]])
  }
  invisible(ok)
}

fcntl_state <- function(handle) {
  res <- .Call("seagull_fcntl_state", handle$fd, PACKAGE="seagull")
  list(locked=as.logical(res[[1]]), pid=res[[2]])
}

fcntl_lock <- function(handle, open) {
  res <- .Call("seagull_fcntl_lock", handle$fd, open, PACKAGE="seagull")
  ret <- res[[1]]
  if (length(res) == 3L) {
    ret <- structure(ret, errno=res[[2]], errmsg=res[[3]])
  }
  ret
}

hack_lock <- function(handle, open) {
  filename <- handle$filename
  if (open) {
    if (file.exists(filename)) {
      FALSE
    } else {
      tmp <- tempfile(pattern=basename(filename), dirname(filename))
      on.exit(file_remove(tmp))
      str <- paste(Sys.info()[["nodename"]], Sys.getpid(), rand_str(10), sep=":")
      writeLines(str, tmp)
      ## NOTE: Not using file.rename here because we can't avoid
      ## overwriting, so
      ##   !file.exists() && file.rename() && identical(readLines())
      ## could succeed on two machines that are _very_ close together in time.
      ##
      ## This approach here is subject to deadlock if somehow both
      ## processes write to the file at the same time.
      file.copy(tmp, filename) && identical(readLines(filename), str)
    }
  } else {
    file_remove(filename)
  }
}
