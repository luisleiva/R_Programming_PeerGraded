## This function obtains the inverse of a square matrix.
## The matrix is store in cache so next time it will be gotten
## from cache saving time not doing recomputing 
## we assume the matrix has inverse.




## The function makeCacheMatriz computes the inverse (solve) 
## of the matrix. 
## The result is store in cache

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x

#gets the inverse matrix and chache the result
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function makes use of the function makecacheMatrix
## to get the inverse of a matrix form cache, 
## but if there is no matrix in cache this function computes
## the inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m



}
