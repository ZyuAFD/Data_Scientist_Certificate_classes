## Put comments here that give an overall description of what your
## functions do
## Two functions provided here for cache the inversed matrix
## Function I:    create a new object with properties of matrix and inversed matrix
##                property function of getinv() and setinv()
##
## Function II:   Cache the inversed matrix for the new matrix objective created in function I


## Write a short comment describing this function
## In this function, a new matrix object is created here with matrix and its inversed matrix
## set as the properties. Each property has two functions, set and get, for input and extract its value.
makeCacheMatrix <- function(x = matrix()) {
      inv=NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Write a short comment describing this function
## In this function, the inversed matrix is cached to the new matrix object. 
## if the inversed matrix is already in the object, it will be extracted directly.
## if the inversed matrix has not been set, it will calculate it and saved in the object.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached inversed matrix")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinv(inv)
      inv
}
