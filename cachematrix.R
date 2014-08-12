## Author: Rafel Mormeneo Melich
## Date:   2014-08-12
## Description: 
## 	   Matrix inversion is usually a costly computation and their may be some benefit
## 	   to caching the inverse of a matrix rather than compute it repeatedly.
## 	   The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## 	   The function cacheSolve returns the inverse of a matrix created with makeCacheMatrix
## 	   with the particularity that if the inverse is in cache instead of computing it again
## 	   the function only returns the stored inverse. Otherwise it computes the inverse of the 
## 	   matrix and it stores the result in cache for future use.



## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function (y) {
		x <<- y
		i <<- NULL
	}
	get <- function() {
		x
	}
	setinverse <- function(inverse) {
		i <<- inverse
	}
	getinverse <- function(){
		i
	}
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## This function computes the inverse of a matrix constructed with the function makeCacheMatrix.
## If the inverse of the matrix has already been computed it is retrieved from cache. Otherwise the
## inverse is computed and stored in chache for future use.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if (!is.null(i)) {
		message("getting cached inverse")
	} else {
		i <- solve(x$get(), ...)
		x$setinverse(i)
	}
	i
}
