## makeCacheMatrix: Function that creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Create a matrix object
    inv <- NULL
    set <- function(y) {
        x <<- y #updates the matrix
        inv <<- NULL #restarts the inverse value in case the matrix is modified
    }
    get <- function() x #get the matrix
    setinverse <- function(inverse) inv <<- inverse #set the inverse in the cache
    getinverse <- function() inv #obtains the inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: Function that computes the inverse of a matrix object created with makeCacheMatrix above
## If the inverse has already been calculated, it returns the cached version of it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse() #obtains the inverse matrix stored in the cache
    if(!is.null(inv)) {
        message("getting cached data") #indicates that the value obtain comes from the cache
        return(inv)
    }
    data <- x$get() #Obtains the matrix
    inv <- solve(data, ...) #Finds the inverse matrix; The function only works for invertible matrices
    x$setinverse(inv) #Sets the inverse matrix in the cache
    inv
}
