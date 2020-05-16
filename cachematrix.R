## The first function creates a special matrix object that caches its inverse.
## The second function computes the inverse of the special matrix returned by first function.


## makeCacheMatrix to create special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve function to compute the inverse of the special matrix returned by makeCacheMatrix.


cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
