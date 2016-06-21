#makeCacheMatrix function takes a square matrix as an argument, and creates an object 
#that can cache its inverse
#

makeCacheMatrix <- function (x = matrix())  {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function () x
  setInverse <- function(matrix)   m <<- matrix
  getInverse <- function() m
  
  list (set=set, get = get, setInverse=setInverse , getInverse = getInverse)
}


#CacheSolve function takes an object as an argument, whichis an  instant of makeCacheMatrix
# It checks to see if the inverse matrix is cached, if cached it will return the cached matrix.
# Ih howevere the there are no cached matrix, it will inverse the matrix and store it in the cache.

cacheSolve <- function (x, ...) {
  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached data")  
    return(m)
  }
  
  data <- x$get()
  m <- solve (data)
  x$setInverse(m)
  m
}
