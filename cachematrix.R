## makeCacheMatrix creates a cacheable matrix as a list
## Functions allow you to get and set the matrix, and also then to get and set its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) i <<- inv
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of the cacheable matrix
## first it checks if the inverse has been cached and if so it returns that value. If the inverse
## is not cached it will calculate the inverse and then store it in the object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
        	return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
