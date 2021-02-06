## Put comments here that give an overall description of what your
## functions do

# Computing an inverse of a matrix can be computationally time consuming. 
# This can be done with help of defining two matrixes.  
# In this assignment two functions. 
.

## Write a short comment describing this function
# This function initialize the inverse to NULL and defines set and get function for 
# the input matrix as well as set and get for the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
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
# This function gets the inverse of the input matrix.
# if the cache is NULL, it calculates the inverse and save it in cache.
# if cache is already calculated, it would return cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getinv()
          if(!is.null(inv)) {
            message("getting cached inverse matrix")
            return(inv)
          }
          data <- x$get()
          inv <- solve(data)
          x$setinv(inv)
          inv
}
