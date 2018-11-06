#Solve for the inverse of a matrix but cache the result, so that if a matrix is
#repeated (in a loop, for instance) the computation doesn't need to run again

#Creates a special matrix object that can cache its inverse and returns a list
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, 
           getinverse = getinverse)
}


#Solves for the inverse of a matrix and returns it if not cached
#Returns the cached inverse value instead of computing if available

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <-x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
