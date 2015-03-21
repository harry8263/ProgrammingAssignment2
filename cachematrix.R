# Pair of functions that cache the inverse of a matrix.

## The below function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){                        ## set value of matrix
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {                        ## get value of matrix 
    x
  }
  
  setinv <- function(inverse) {              ## set the value of inv  
    inv <<- inverse
  }
  
  getinv <- function() {                     ## get the value of inv
    inv
  }  
  
  list(a = set, b = get, 
       c = setinv, d = getinv)               ## creates a list of functions
  
}

## The below function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$d()                             ## get the inverse
  
  if(!is.null(inv)){                       ## if the inverse is already calc     
    ## and in cache, pull it          
    message("getting cached data...")
    return(inv)
    
  }
  
  data <- x$b()                           ## if the inverse is not calc     
  inv <- solve(data, ...)                 ## get the matrix and find the inverse
  x$c(inv)                                ## and store it in cache
  inv
  
}
