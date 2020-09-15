## This first function, creates a matrix, which is really a list containing a function to cache the inverse of the matrix
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL  ## initializing inverse as NULL
  set <- function(y) {  ##function to set the value of the matrix
    x <<- y
    i <<- NULL
  }
  ##function to define the return matrix object or the value of the matrix
  get <- function() {x}
  ##setting the inverse matrix
  setinv <- function(inverse)i<<-inverse
  ##function for getting the inverse matrix
  getinv <- function(){i}
  
  ## put all defined function into a list
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##The next function compute the inverse of the matrix, if it already exist. The function will calculate the inverse of the matrix.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {  ## gets cache data
  i <- x$getinv()
  ## next: Verified if the inverse has already been calculated
  if(!is.null(i)){
    
    ##is the return matrix identical?
    if(identical(x$get() %*% i, i %*% x$get())){
      message("getting cached data")
      return(i)   ## returns the inverse value
    }
  }
  
  ##calculates the inverse
  
  data <-x$get()
  i <-solve(data, ...)   ##calculates the inverse data
  x$setinv(i)
  return(i)  ## return a matrix that is the inverse of "x"
}

