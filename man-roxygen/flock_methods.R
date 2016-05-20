##' @section Methods:
##'
##' \describe{
##' \item{\code{acquire}}{
##'   Attempt to acquire a lock on the file.  For details on arguments see the main argument section above.
##'
##'   \emph{Usage:}
##'   \code{acquire(delay = 0.01, max_delay = 0.1, timeout = Inf, error = TRUE,
##'       verbose = FALSE)}
##'
##'   \emph{Arguments:}
##'   \itemize{
##'     \item{\code{delay}:   Initial delay
##'     }
##'
##'     \item{\code{max_delay}:   Maximum delay between attempts
##'     }
##'
##'     \item{\code{timeout}:   Maximum time to wait
##'     }
##'
##'     \item{\code{error}:   Throw an error on failure?
##'     }
##'
##'     \item{\code{verbose}:   Print information while attempting to acquire lock?
##'     }
##'   }
##' }
##' \item{\code{release}}{
##'   Release the lock.
##'
##'   \emph{Usage:}
##'   \code{release()}
##' }
##' }
