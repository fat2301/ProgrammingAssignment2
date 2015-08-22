## cachematrix.R Allows to store and cache the inverse of a matrix. 

## makeCacheMatrix creates a matrix that stores a matrix of values, and a list of functions 
## that enable it to store (cache) the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  myinv <- NULL
  set <- function(y = matrix()) {
    x <<- y
    myinv <<- NULL
  }
  get <- function() x
  setinv <- function(theinv) myinv <<- theinv
  getinv <- function() myinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSOlve uses Solve to inverse a matrix. It first checks if the inverse is stored 
## in the input matrix. If an inverse value already exists in the matrix object , it returns the inverse.
## If it doesn't exist, it will Calculate the inverse and then stores it into the object. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  myinv <- x$getinv()
  if(!is.null(myinv)) {
    message("getting cached inverse matrix")
    return(myinv)
  }
  inputmatrix <- x$get()
  myinv <- solve(inputmatrix)
  x$setinv(myinv)
  myinv
  
}


