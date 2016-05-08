#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

#Computing the inverse of a square matrix can be done with the solve function in R. 
#For example, if X is a square invertible matrix, then solve(X) returns its inverse.

#setInverse - sets the value of the matrix
#getInverse - gets the value of the matrix

#setInverse - sets the value of the inverse
#getInverse - gets the value of the inverse


## This function caches a value "i"

makeCacheMatrix <- function(x = matrix()) {
        # Initialize "i" with null.
        i <- NULL
        
        #Assign a matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        #Retrieve matrix
        get <- function() x
        
        #Cache matrix
        setInverse <- function(inverse) i <<- inverse
        
        #Retrieve matrix
        getInverse <- function() i
        
        #Return matrix for function makeCacheMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}




## Return a matrix that is the inverse of 'x'
# This function will calculate the inverse of a matrix
cacheSolve <- function(x, ...) {
        
        #Retrieve cached value
        i <- x$getInverse()
        
        #If value retrieved was cached, print "getting cached data".
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        #If value was not cached, then calculate the inverse of the matrix
        data <- x$get()
        #use solve function in R to compute matrix inverse
        i <- solve(data, ...)
        x$setInverse(i)
        
        #Return inverse value for function cacheSolve
        i
}        
