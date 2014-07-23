## These functions perform a caching function for matrix inversion
## that checks for a cached copy of the inversion before going through the 
## calculation

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
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


## This function computes the inverse of the special "matrix" returned by
## the makeCacheMatrix function above. If the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve function
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

## Notes from Community TA on testing
## 
## amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## amatrix$get()         # Returns original matrix
## cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
## amatrix$getinverse()  # Returns matrix inverse
## cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
## 
## amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
## cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
## amatrix$get()         # Returns matrix
## amatrix$getinverse()  # Returns matrix inverse

