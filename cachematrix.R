## These functions use lexical scoping to calculate, store
## and return the inverse of a matrix

## This function makes a list of the set and get of a matrix,
## as well as the set and get of the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setinverse <- function(solve)inv <<- inv
        getinverse <- function()inv
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## This function calculates the inverse of a matrix but checks
## to see if it has been calculated already.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}

