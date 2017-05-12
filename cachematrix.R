        ## makeCacheMatrix can find and cache the inverse of a matrix
        ## NOTE - assumes input (x) is an invertabile square matrix
        makeCacheMatrix <- function(x = matrix()) {
                invmat <- NULL
                set <- function(y) {
                        x <<- y
                        invmat <<- NULL
                }
                get <- function() x
                setinverse <- function(inv) invmat <<- inv
                getinverse <- function() invmat
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }

        ## CacheSolve will return the inverse of the matrix. It will use cache'd copy 
        ## if it exists in cache, otherwise it computes the inverse using solve function
        cacheSolve <- function(x, ...) {
                invmat <- x$getinverse()
                if(!is.null(invmat)) {      
                        message("getting cached data")
                        return(invmat)
                }
                data <- x$get()
                invmat <- solve(data, ...)
                x$setinverse(invmat)
                invmat
        }
        
