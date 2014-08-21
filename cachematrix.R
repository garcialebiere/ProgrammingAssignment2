## This functions cache the inverse of a matrix. Matrix inversion is usually a
## costly computation and their may be some benefit to caching the inverse of a
## matrix rather than compute it repeatedly.

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.
## The function takes a numeric matrix as parameter (for the assignment, assume
## that the matrix supplied is always invertible) and return a list wormed by 
## several functions: set, get, setinverse and getinverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
		##This function is used to set the value of the cachedMatrix
        set <- function(y){
                x<<-y
                inv<<-NULL
        }
		##This function returns the value of the cachedMatrix
        get<-function(){
                x
        }
		##This function set the value of the inverse of the cachedMatrix
        setinverse<-function(inverse){
                inv<<-inverse
        }
		##This function returns the value of the inverse of the cachedMatrix
        getinverse<-function(){
                inv
        }
		##Return a list of these functions
        list(set = set, get = get, 
			setinverse = setinverse, 
			getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve should 
## retrieve the inverse from the cache. If the cachedMatrix hasn't the value
## of the inverse cached, the function calculate and save it.
cacheSolve <- function(x, ...) {
        ## Take the value of the inverse matrix from the cacheMatrix
		inverseMatrix <- x$getinverse()
		## If this value has computed yet, return the value
        if(!is.null(inverseMatrix)){
                message("getting cached data")
                return(inverseMatrix);
        }
		## If the inverse matrix hasn't cached yet, take the stored matrix,
		## compute the inverse matrix and set it in the cachedMatrix
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setinverse(inverseMatrix)
		## Return a matrix that is the inverse of 'x'
        inverseMatrix
}
