## Two functions to cache the inverse of a matrix.

## 1.- makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matri

makeCacheMatrix <- function(x = matrix()) {
        inversa <- NULL
        set <- function(y) {
                x <<- y
                inversa <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inversa <<- inverse
        getinverse <- function() inversa
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## 2.- cacheSolve: 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inversa <- x$getinverse()
        if(!is.null(inversa)) {
                message("Allready Calculated.")
                return(inversa)
        }
        message("Calculating, 1° run")
        data <- x$get()
        inversa <- solve(data)
        x$setinverse(inversa)
        inversa
}
