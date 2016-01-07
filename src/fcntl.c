#define STRICT_R_HEADERS 1
#include <R.h>
#include <Rinternals.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

static void seagull_finalize(SEXP extPtr);
int* seagull_get_fd(SEXP extPtr, int closed_error);

SEXP seagull_open(SEXP r_filename) {
  const char* filename = CHAR(STRING_ELT(r_filename, 0));
  int *fd = (int*) R_Calloc(1, int);
  // From fasteners:
  //
  //   Open in append mode so we don't overwrite any potential
  //   contents of the target file. This eliminates the
  //   possibility of an attacker creating a symlink to an
  //   important file in our lock path.
  *fd = open(filename, O_CREAT | O_RDWR | O_APPEND, 0666);
  if (*fd == -1) {
    Rf_error(strerror(errno));
  }

  SEXP extPtr = PROTECT(R_MakeExternalPtr(fd, R_NilValue, R_NilValue));
  R_RegisterCFinalizer(extPtr, seagull_finalize);
  UNPROTECT(1);
  return extPtr;
}

SEXP seagull_close(SEXP extPtr, SEXP closed_error) {
  int *fd = seagull_get_fd(extPtr, INTEGER(closed_error)[0]);
  int ok, errsv;
  SEXP ret = R_NilValue;
  if (fd) {
    ok = close(*fd) == 0;
    errsv = errno;
    ret = PROTECT(allocVector(VECSXP, ok ? 1 : 3));
    SET_VECTOR_ELT(ret, 0, ScalarLogical(ok));
    if (ok) {
      R_Free(fd);
      R_ClearExternalPtr(extPtr);
    } else {
      SET_VECTOR_ELT(ret, 1, ScalarInteger(errsv));
      SET_VECTOR_ELT(ret, 2, mkString(strerror(errsv)));
    }
    UNPROTECT(1);
  }
  return ret;
}

void seagull_finalize(SEXP extPtr) {
  seagull_close(extPtr, ScalarInteger(0));
}

int* seagull_get_fd(SEXP extPtr, int closed_error) {
  int *fd = NULL;
  if (TYPEOF(extPtr) != EXTPTRSXP) {
    Rf_error("Expected an external pointer");
  }
  fd = (int*) R_ExternalPtrAddr(extPtr);
  if (!fd && closed_error) {
    Rf_error("File handle already closed");
  }
  return fd;
}

SEXP seagull_fcntl_lock(SEXP extPtr, SEXP r_open) {
  int fd = *seagull_get_fd(extPtr, 1), open = INTEGER(r_open)[0], ok;
  SEXP ret;
  int errsv;
  struct flock fl;
  fl.l_type   = open ? F_WRLCK : F_UNLCK;
  fl.l_whence = SEEK_SET;
  fl.l_start  = 0;
  fl.l_len    = 0;
  fl.l_pid    = getpid();
  ok = fcntl(fd, F_SETLK, &fl) == 0;
  errsv = errno;
  ret = PROTECT(allocVector(VECSXP, ok ? 1 : 3));
  SET_VECTOR_ELT(ret, 0, ScalarLogical(ok));
  if (!ok) {
    SET_VECTOR_ELT(ret, 1, ScalarInteger(errsv));
    SET_VECTOR_ELT(ret, 2, mkString(strerror(errsv)));
  }
  UNPROTECT(1);
  return ret;
}

SEXP seagull_fcntl_state(SEXP extPtr) {
  int fd = *seagull_get_fd(extPtr, 1), ok, locked;
  SEXP ret = PROTECT(allocVector(INTSXP, 2));
  int *iret = INTEGER(ret);
  int errsv;
  struct flock fl;
  fl.l_type   = F_WRLCK;
  fl.l_whence = SEEK_SET;
  fl.l_start  = 0;
  fl.l_len    = 0;
  fl.l_pid    = getpid();
  ok = fcntl(fd, F_GETLK, &fl) == 0;
  if (!ok) {
    Rf_error(strerror(errno));
  }
  iret[0] = fl.l_type != F_UNLCK;
  iret[1] = fl.l_pid;
  UNPROTECT(1);
  return ret;
}
