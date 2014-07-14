## With lexical scoping, R has the ability to cache time-consuming computations.
## Instead of dumping the result of this tedious computation, it is stored 
## in memory so that the value is ready to be used again by the application.
## The problem given stores an inverse of a matrix into the cache.

## To achieve this, two functions are declared:
## One function for creating the special matrix
## And one function for checking the cached inverse

## The first function creates a special matrix that:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## sets the matrix
        get <- function() x
        ## Gets the matrix
        setinverse <- function(solve) m <<- solve
        ## Sets the inverse of a matrix
        getinverse <- function() m
        ## Get the inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        ## List the results of the four processes
}

## The second function calculates the inverse of a matrix.
## Unlike the predefined function, it checks the cache if the
## given matrix has already an inverse stored. It immediately
## returns the inverse in this case. Otherwise, the function
## solves the value.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        ## Gets the inverse of 'x' that is in the cache
        if(!is.null(m)) {
                ## If there is an existing value
                message("getting cached data")
                return(m)
                ## Return the inverse of 'x'
        }
        data <- x$get()
        m <- solve(data, ...)
        ## Solves for the inverse of 'x'
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}