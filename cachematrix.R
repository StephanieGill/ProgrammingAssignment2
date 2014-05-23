##These functions cache the inverse of a matrix.

##Creates a special "matrix" object
makeCacheMatrix <- function(x = matrix(), ...) {  
    i <- NULL
    set <- function(y) {     ##set the value of the matrix
            x <<- y
            i <<- NULL
    }
    get <- function() x   ##get the value of the matrix
    setinverse <- function(solve) i <<- solve   ##set the inverse of the matrix
    getinverse <- function() i   ##get the inverse of the matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##Computes the inverse of the matrix created above
cacheSolve <- function(x, ...) {
    if(!is.null(i)) {   ##checks to see if the inverse has already been calculated
          message("getting cached data")
          return(i)   ##if it has, then it returns the inverse
    }
    data <- x$get()   ##calculates the inverse of the matrix
    i <- solve(data, ...)
    x$setinverse(i)   ##sets the value of the inverse in the cache
    i
}
