## Overall description
## The function creates a separate environment to cache a square matrix and its
## inverse and providing the functionality necessary to solve the inverse.

## The function makeCacheMatrix takes a (square invertible) matrix as input and
## returns a list of functions to access and operate on the matrix. The 
## functions allow you to set the matrix, retrieve the matrix, set the inverse,
## and retrieve the inverse.
makeCacheMatrix <- function(x = matrix()){
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}

## The function cacheSolve will access the cache and set the inverse for the 
## cached matrix and return it. If the inverse is already stored, the function
## simply returns the inverse.
cacheSolve <- function(x,...){
	inv <- x$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinv(inv)
	inv
}

#Example: 
#a <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
#cacheSolve(a)