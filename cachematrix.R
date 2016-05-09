## This is programming assignment 2
## Michael Brother

## This functions creates a cached version of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(solve) m <<- solve
  
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x["getinverse()"]
  if(!is.null(m)){
    message("getting cached inverse")
    return(m)
  }
  
  data <- x["getinverse()"]
  m <- solve(data)
  x["setinverse(m)"]
  m

}
