# the following two functions are used to cache the inverse of a matrix

# makeCacheMatrix creates a special "matrix" object that can cache its inverse
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
# the function assumes that the matrix is always invertible

cacheSolve <- function(x, ...) {
  
    m <- x$getInverse()
    if(!is.null(m)) {
      print("getting cached data")
      return(m)
    }
    m <- solve(x$get)
    x$setInverse(m)
    m
  
}

