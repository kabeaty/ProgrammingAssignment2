## Constructs a matrix object capable of caching its inverse matrix
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


## Returns the inverse of the matrix
## Either calculates the inverse matrix, or grabs the inverse matrix from
## cache and returns it from there if previously calculated
cacheSolve <- function(x, ...) {
        inversematrix <- x$getinverse()
        if(!is.null(inversematrix)) {
                message("retrieving cached matrix inverse")
                return(inversematrix)
        }
        matrix <- x$get()
        inversematrix <- solve(matrix)
        x$setinverse(inversematrix)
        inversematrix
}
