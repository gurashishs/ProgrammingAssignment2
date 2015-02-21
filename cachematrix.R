## The functions below are to help solve for the inverses of 
## large matrices. These functions make the process more efficient
## by caching the solution for the inverse

## Desc: Creates a special object with a matrix and its inverse, 
## each with getters and setters.

makeCacheMatrix <- function(x = matrix()) {
	xInv <- NULL
	set <- function(xNew){
		x <- xNew
		xInv <- NULL
	}
	get <- function() x
	setInverse <- function(xInvNew) xInv <<- xInvNew
	getInverse <- function() xInv
	list(set = set, get=get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## Takes a special matrix object and either calculates its inverse
## and stores it or retrieves a stored inverse if it exists

cacheSolve <- function(x, ...) {
    xInv <- x$getInverse()
    if (!is.null(xInv)){
    	message("Getting cached data")
    	return(xInv)
    }
    xData <- x$get()
    xInv <- solve(xData,...)
    x$setInverse(xInv)
    xInv
}