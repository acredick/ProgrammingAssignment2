## Finds the inverse of a matrix and stores it in cache


## Cache of a matrix & its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        ## set matrix val
        inverse1 <- NULL
        set <- function(y) {
                x <<- y
                inverse1 <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse2) inverse1 <<- inverse2
        getinverse <- function() inverse1
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Fetches inverse from cache or finds matrix and puts it in cache
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

matrix <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
x <- makeCacheMatrix(matrix)
print(cacheSolve(x))

