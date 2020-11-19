makeCacheMatrix <- function(x= matrix) {
  ##makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
  i <- NULL
  set <- function(y)  {
    x <<- y
    i <- NULL
}
  get <- function()x
  setinverse <- function(inverse)i <<- inverse
  getinverse <- function()i
  list(set=set, get=get, setinverse = setinverse, getinverse=getinverse)
}
cacheSolve <- function(x, ...)  {
  i <- x$getinverse ()
  if(!is.null(i)) {
    message("getting catched data")
    return(i)
  }
  data <- x$get()
  i<- solve(data, ...)
  x$setinverse(i)
  i
}
##cacheSolve: Computes the inverse of the matrix