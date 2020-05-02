## The following function will cache the inverse of a matrix using the scoping 
## rules of the R language, therefore to avoid time-consuming computations. If 
## the contents of a matrix are not changing, we can just cache the inverse of 
## the matrix, we can then look up it in the cache when we need it again. The 
## following two functions are used to cache the inverse of a matrix. 

## makeCacheMatrix creates a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matirx
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function returns the inverse of the matrix created with the 
## above function. However, it first checks to see if the inverse has already 
## been computed. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it computes the inverse of the data and sets the value ## of the inverse in the cache via the setinverse function. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

