

## 1.  `makeCacheMatrix`: This function creates a special 'matrix' object that can
## cache its inverse.  2.  `cacheSolve`: This function computes the inverse of the
## special 'matrix' returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then `cacheSolve`
## should retrieve the inverse from the cache.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # set to NULL to start
    inv <- NULL

    # set the matrix inverst is still NULL
    setmatrix <- function(new_X) {
        x <<- new_X
        inv <<- NULL
    }
    # Get the matrix
    getmatrix <- function() x

    #set the inverse
    setinverse <- function(inverse) inv <<- inverse

    #Get inverse
    getinverse <- function() inv

    #Encapsulate into a list
    list(setmatrix = setmatrix,
         getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
 if(!is.null(inv)) {
         message("getting cache")
         return(inv)
 }
 data <- x$getmatrix()
 inv <- solve(data, ...)
 x$setinverse(inv)
 inv
}

## Usage example:
## x <- matrix(2:7, nrow=2, ncol=2)
## mat <- makeCacheMatrix(x)
## sol <- cacheSolve(mat)
## sol
## sol returns:
##          [,1] [,2]
##    [1,] -2.5    2
##    [2,]  1.5   -1
##
## sol2 <- cacheSolve(mat)
## This displays a "Getting cached matrix" message
## sol2
## sol2 returns:
##          [,1] [,2]
##    [1,] -2.5    2
##    [2,]  1.5   -1
# # https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md
