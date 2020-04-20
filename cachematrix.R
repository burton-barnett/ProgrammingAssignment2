## makeCacheMatrix is a set of functions to cache the inverse of an invertible
## matrix. cacheSolve checks for a cached inversion and inverts if no inversion
## is cached.

## makeCacheMatrix sets im (short for inverse matrix) to NULL.  It establishes
## the functions to get the original matrix, solve for the inverse matrix, and
## fetch the inverse matrix if it is stored as im.

## Be sure to use an invertible matrix here.  You can check with det(x) if x is
## a square matrix (nrow = ncol).  det(x) is != 0 if it is invertible.

## make an object y that is makeCacheMatrix(x).  This is passed to cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        if(det(x) == 0) {
                message("Only for invertible matrices, fool!")
        }
        im <- NULL
        getmatrix <- function() x
        setinversematrix <- function(solve) im <<- solve
        fetchinverse <- function() im
        list(getmatrix = getmatrix,
             setinversematrix = setinversematrix,
             fetchinverse = fetchinverse)
}


## cacheSolve will invert the matrix x if the inversion of x is not already stored as im.

cacheSolve <- function(y, ...) {
        im <- y$fetchinverse()
        if(!is.null(im)) {
                message("You already inverted this matrix! I'll go get the inversion.")
                return(im)
        }
        data <- y$getmatrix()
        im <- solve(data, ...)
        y$setinversematrix(im)
        im
}

## if you make an object z that is cacheSolve(y), you can check the
## inversion with zapsmall(x %*% z).  It should return the identity matrix.