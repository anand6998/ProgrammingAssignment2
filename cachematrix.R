## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# 
# Creates a matrix which and exposes the inverse of the matrix which is cached 
# after the first computation
# c=rbind(c(1, -1/4), c(-1/4, 1))
# l <- makeCacheMatrix (c)
makeCacheMatrix <- function (x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function (inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
# calculates the inverse of a matrix
# if the matrix inverse is cached then returns the cached value
# cacheSolve (l)
#           [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return (inv)
  }
  
  matrix <- x$get()
  inv <- solve(matrix)
  x$setInverse(inv)
  inv
}
