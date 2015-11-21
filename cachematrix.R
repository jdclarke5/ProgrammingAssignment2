## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
# 1. get the value of the matrix
# 2. set the value of the inverse
# 3. get the value of the inverse

# cacheSolve calculates the inverse of the special "matrix" created with makeCacheMatrix. 
# It first checks to see if the inverse has already been calculated. 
# If so, it returns from the cache.
# Otherwise, it calculates the inverse and stores the inverse in the cache.


## Write a short comment describing this function

# input is a matrix, e.g. in R Console:
# > mat <- matrix(c(1,3,5,7,1,2,3,5,3),ncol=3)
# > cmat <- makeCacheMatrix(mat)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(get = get,
    setinv = setinv,
    getinv = getinv)
}


## Write a short comment describing this function

# input is a makeCacheMatrix and remaining inputs for solve(), e.g. in R Console:
# > mat <- matrix(c(1,3,5,7,1,2,3,5,3),ncol=3)
# > cmat <- makeCacheMatrix(mat)
# > cacheSolve(cmat)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
