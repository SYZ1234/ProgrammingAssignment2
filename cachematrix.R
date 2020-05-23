## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#function1: makeCacheMatrix
makeCacheMatrix <- function(x= matrix()) {#
        inv <- NULL
        set <- function(m){
                x <<- m
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(solve) inv <<- solve
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

#An example to test the function 'makeCacheMatrix'
z=makeCacheMatrix(matrix(c(11,12,13,14), nrow = 2, ncol = 2));
z$get()



## Write a short comment describing this function
#function2: cacheSolve
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)){
                message("Cached data here!")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInv(inv)
        inv      
}

#An example to test the function cacheSolve
cacheSolve(z)
