makeCacheMatrix <- function(x = matrix()) {
  
  xinv<- NULL
  set<-function(y) {
    x<<-matrix(y)
    xinv<<- NULL
  }
  get <- function() x
  setinv<- function(matinv) xinv<<-matinv
  getinv<- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function that solves for the inverse
## first determines if inverse has already been calculated and cached
## if not calculates inverse using solve()
## if already calculate retrieves cached inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinv()
  if (!is.null(xinv)) {
    message("Getting cached data ...")
    return(xinv)
  }
  data <-x$get()
  xinv<-solve(data)
  x$setinv(xinv)
  xinv
}