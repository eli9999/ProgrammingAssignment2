## MakeCacheMatrix: Creates a special matrix object that can cache its inverse. 
## If it was already calculated it it retrieves it from the cache.

  makeCacheMatrix <- function(x = matrix()) {
      inv_x <- NULL
      set <- function(y) {
          x <<- y
          inv_x <<- NULL
     }
      get <- function() x
      setinverse<- function(inverse) inv_x <<-inverse
      getinverse <- function() inv_x
      list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
     }
 

 ## cacheSolve returns the inverse of a matrix made by makeCacheMatrix()
 ## If the cached inverse is available, cacheSolve retrieves it, otherwise calculates, caches and returns.
  
  cacheSolve <- function(x, ...) {
     
      inv_x <- x$getinverse()
      if (!is.null(inv_x)) {
          message("getting cached inverse matrix")
         return(inv_x)
     } else {
         inv_x <- solve(x$get())
         x$setinverse(inv_x)
         return(inv_x)
     }
    }
