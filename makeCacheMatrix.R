makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get<- function() {x}
  setIverse <- function(inverse) {inv <<- inverse}
  getIverse <- function() {inv}
  list(set = set, get = get, setIverse = setIverse, getIverse = getIverse)
}

cachesolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}