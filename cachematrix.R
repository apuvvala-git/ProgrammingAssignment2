# File contains two functions that computes inverse of a given matrix
# and caches it for faster access for subsequent queries

# makeCacheMatrix - Makes a special matrix, a list of functions that
# (a) sets the original matrix
# (b) gets the original matrix
# (c) sets the matrix inverse
# (d) gets the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) minv <<- inv
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  

}


# cacheSolve - Solves inverse of a matrix and caches its value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  if(!is.null(minv)) {
    message("getting cached matrix inverse")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setinv(minv)
  minv
}
