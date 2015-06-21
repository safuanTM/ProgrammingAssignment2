## Put comments here that give an overall description of what your
## functions do
##
## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        ##      set the 'cache' variable
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ##      get the matrix
        get <- function() x
        
        ##      set the inverse
        setinverse <- function(solve) m <<- solve
        
        ##      get the inverse
        getinverse <- function() m
        
        ##      prepare the list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve computes the inverse of the 
## special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## if the inverse is not NULL, just return m from the cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if it is NULL, need to work it back again. E.g. calculate the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
