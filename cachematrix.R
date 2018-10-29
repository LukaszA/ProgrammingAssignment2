## create a special matrix

makeCacheMatrix <- function(x = matrix()) {

  matInv <- NULL
  set <- function(y) {
    x <<- y
    matInv <<- NULL
  }
  get <- function() x
  setMatInv <- function(solve) matInv <<- solve #save inversed matrix
  getMatInv <- function() matInv                #get cached inversed matrix
  list(set = set, get = get,
       setMatInv = setMatInv,
       getMatInv = getMatInv)
}



## compute inverse of the special matrix

cacheSolve <- function(x= matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  MatInv <- x$getMatInv()
  if(!is.null(MatInv)) {              #check cached matrix
    message("getting cached data")
    return(MatInv)
  }
  data <- x$get()
  MatInv <- solve(data, ...)
  x$setMatInv(MatInv)
  MatInv
}
