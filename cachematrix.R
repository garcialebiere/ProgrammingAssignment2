## Put comments here that give an overall description of what your
## functions do:
## This functions cache the inverse of a matrix. Matrix inversion is usually a
## costly computation and their may be some benefit to caching the inverse of a
## matrix rather than compute it repeatedly.

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function(){
                x
        }
        setinverse<-function(inverse){
                inv<<-inverse
        }
        getinverse<-function(){
                inv
        }
        list(set = set, get = get, 
			setinverse = setinverse, 
			getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverseMatrix <- x$getinverse()
        if(!is.null(inverseMatrix)){
                message("getting cached data")
                return(inverseMatrix);
        }
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setinverse(inverseMatrix)
        inverseMatrix
}
