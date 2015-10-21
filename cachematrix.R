## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inversematrix <- NULL
        set <- function(y) {
                x <<- y
                inversematrix <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inversematrix <<- solve
        getinverse <- function() inversematrix
        list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inversematrix <- x$getinverse()
        if(!is.null(inversematrix)) {
                message("retrieving cached matrix inverse")
                return(inversematrix)
        }
        matrix <- x$get()
        inversematrix <- solve(matrix)
        x$setinverse(x)
        inversematrix
}
