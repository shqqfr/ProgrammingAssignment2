## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than computing it repeatedly. Here we wirte a 
## pair of functions that cache the inverse of a matrix.

## The following functions are included:
## makeCacheMatrix: this function creates a special "matrix" object that can cache its inverse.

## example
##      > source("cachematrix.R")
##      > x=matrix(1:4, 2, 2);
##      > m=makeCacheMatrix(x)
##      > m$get()

##      the first run
##      > cacheSolve(m)
##      
##              [,1] [,2]
##      [1,]   -1   -1
##      [2,]   -2   -1

##      Retrieving from the cache
##      > cacheSolve(m)
##      getting cached data
##              [,1] [,2]
##      [1,]   -1   -1
##      [2,]   -2   -1

makeCacheMatrix <- function(x = matrix()) {
        mycache <- NULL
        
        set <- function(y) {
                x <<- y
                mycache <<- NULL
        }
        
        get <- function() x
        setmymatrix <- function(inverse) mycache <<- inverse
        getmyinverse <- function() mycache
        
        list(set = set, get = get,
             setmymatrix = setmymatrix,
             getmyinverse = getmyinverse)
}

## cacheSolve: this function computes the inverse of the special "matrix" returned by
##    makeCacheMatrix above. If the inverse has already been calculated (and the matrix has
##    not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mycache <- x$getmyinverse()
        
        if(!is.null(mycache)) {
                message("getting cached data")
                return(mycache)
        }
        
        mymatrix <- x$get()
        
        mycache <- solve(mymatrix, ...)
        
        x$setmymatrix(mycache)
        mycache
}
