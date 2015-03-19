## cachematrix.R
##
## This R file is to implement calculating the inverse of a matrix 
## based on cache scheme.
##
## Usage:
##  Initialize M to a reversible matrix
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheSolve(cacheMatrix)      ##calculate inverse of matrix M first time
##  cacheSolve(cacheMatrix)      ##return cached inverse of matrix M, no need to calculate inverse of matrix M again
##  Initialize N to a reversible matrix, and different from matrix M
##  cacheMatrix$set(N)           ##assign to a new matrix value N
##  cacheSolve(cacheMatrix)      ##cached inverse of previous matrix M is invalid, need to calculate inverse of matrix N again
##  cacheSolve(cacheMatrix)      ##return cached inverse of matrix N, no need to calculate inverse of matrix N again

## This function creates a special "matrix" object that can cache its inverse,
## including matrix object get/set method, and the matrix inverse get/set method.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
