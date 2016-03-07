# Take matrix as argument or generate empty matrix and return list
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

# Take cachable matrix list and calculate and store matrix inverse
cacheinverse <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting inverse data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
