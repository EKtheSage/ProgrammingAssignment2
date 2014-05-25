## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## initialize ma to NULL
        ma <- NULL
        
        ## set function takes in a matrix, and set x equal to it
        ## also set ma to NULL
        set <- function(y) {
                x <<- y
                ma <<- NULL
        }
        
        ## get function just returns x that was assigned in set(y)
        get <- function() x
        
        ##setcache function sets ma equal to cache
        setcache <- function(cache) ma <<- cache
        
        ##getcache function just returns ma
        getcache <- function() ma
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ##getcache of matrix x. if it is not NULL
        ##then use the cached inverted matrix
        ma <- x$getcache()
        if(!is.null(ma)) {
                message("getting cached data")
                return(ma)
        }
        
        ##if it is not NULL, get() to get the matrix that was 
        ##input in the makeCacheMatrix
        data <- x$get()
        
        ##assign ma to inverted matrix, set it to cache
        ma <- solve(data, ...)
        x$setcache(ma)
        ma
}
