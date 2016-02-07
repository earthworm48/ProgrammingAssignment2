## Assignment 2: Computing the inverse of a square matrix
## Caching the matrix so the computation do not need to be repeated if 
## the calculation have been done before
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setmatrix <- function(inversematrix) im <<- inversematrix
  getmatrix <- function() im
  
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## if the function have been calculated before, then the inverse 
## value of the matrix would be returned, else, the computation will
## be done

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getmatrix()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setmatrix(im)
  im
}
