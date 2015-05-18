## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly 


## makeCacheMatrix creates a placeholder for the cahed value. 
## It contains 2 holders for the original matrix and for it's cached inverse (mx and solve)
## and also has 4 functions: a 2 set/get pairs for each  holder. The set of the matrix object is also resetting 
## the cached value of the inverse
## publicly is exposing just the 2 set/get pairs: set,get,setsolve,getsolve

makeCacheMatrix <- function(mx = matrix()) {
    solve <- NULL
    
    ## set/get for the matrix object
    set <- function(mxValue) {
        mx <<- mxValue
        solve <<- NULL
    }
    get <- function() mx
    
    ## set/get for the cached inverse matrix
    setsolve <- function(solveValue) solve <<- solveValue
    getsolve <- function() solve
    
    ## return the access functions 
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}


## cacheSolve returns a matrix that is the inverse of x
## it the inverse of x was not calculated before, it will be calculated and then cached
## it is was calculated and cached, it will be returned without need for recalculation

cacheSolve <- function(x, ...) {
    ## try to get the cached inverse
    solve <- x$getsolve()
    if(!is.null(solve)) {
        ## the inverse matrix was cached. We can use the cached value
        message("getting cached data")
        return(solve)
    }
    
    ## the inverse was not cached. It has to be calculated
    ## get the original object
    data <- x$get()
    ##calculate the inverse matrix
    solve <- solve(data, ...)
    ##cache the inverse matrix
    x$setsolve(solve)
    
    ## return the inverse matrix
    solve
    
}
