## These functions are used to cache the inverse of a matrix which would otherwise
## be a very costly and time-consuming operation

## This function is used to cache the inverse and the original matrix for 
## repeated use

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  storedMatrix <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse){
    inv <<- inverse
    storedMatrix <<- x
  }
  getinverse <- function() inv
  getstoredmatrix <- function() storedMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       getstoredmatrix=getstoredmatrix)
}


## This function is used to check if cached inverse exists and the matrix is unchanged
##  else it recalculates the inverse of the given matrix

cacheSolve <- function(x=matrix(), ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    if((nrow(x$getstoredmatrix())==nrow(x$get())) && 
       (ncol(x$getstoredmatrix())==ncol(x$get())))
    {
      if(identical(x$getstoredmatrix(),x$get()))
      {
        message("getting cached data")
        return(inv)
      }
    }
    
  }
  data <- x$get()
  newInv <- solve(data)
  x$setinverse(newInv)
  newInv
}
