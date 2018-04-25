##The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ##This function creates a special "matrix" object that can cache its inverse.
        
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## The function calculates the mean of the special "matrix" created with the above function. 
        ## However, it first checks to see if the inverse has already been calculated. 
        ## If so, it gets the inverse from the cache, displays a message and skips the computation. 
        ## Otherwise, it calculates the inverse of the matrix 
        ## and sets the value of the inverse via the setinverse function.
        
        
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("Able to use cache")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
