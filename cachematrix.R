## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function will use closure to store the value of x and it's inverted matirix.
## it will return four functions for setting and getting it's matrix and inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invx<<-inv
  getinv <- function() invx
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## This function will use the cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

testFunc <- function() {
  m = matrix(1:4, 2, 2)
  cached_m = makeCacheMatrix(m)
  print(m)
  print(cacheSolve(cached_m))
  print(cacheSolve(cached_m)) # it will print "getting cached matrix"
  print(cacheSolve(cached_m) %*% m)
}