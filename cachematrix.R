# Assignment: Caching the Inverse of a Matrix
# ===========================================


## These functions abstract away the caching and solving of a matrix's inverse. 
# makeCacheMatrix creates the special matrix object in the global namesace / environment
# cacheSolve looks for the value of a matrix's inverse in the cache, and solves it if it is not null.


#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { #pass the matrix in as an argument      
        d <- NULL
        cm <- NULL       
        set <- function(y) { #set matrix 'y' as the cached matrix
                if (!identical(x,y)) {
                        message("This matrix is not the same as the cached version.
                                Now updating cache...")
                        cm <<- 1 # setting cm a value other than null indicates matrix is not in sync with the cache
                }
                x <<- y
                d <<- NULL
        }       
        get <- function() { x } #get the value of the cached matrix
        setinverse <- function(inverse) { d <<- inverse } #set the 
        getinverse <- function() { d }
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
        }

# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix
# has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x = numeric(), ...) {
        data <- x$get() #gets the cached matrix
        d <- x$getinverse() #gets the value for the inverse of the matrix
        if(!is.null(d)) {#if the value of the inverse is NOT null
                if(!is.null(cm)) {
                        message("Getting cached data...")
                        return(d)
                }
        } else { # value of the inverse is null       
                #Notify user that inverse is not cached, and resolve it.
                message("Inverse was not available from cache. It has been calculated below:")
                d <- solve(data, ...)
                x$setinverse(d)
                d
        }       
}