## This R file contains a set functions which demonstrate how to compute and
## cache inverse of a matrix and retrieve for subsequent use
##
## function makeCacheMatrix is used to create and initialize R object which will
## cache the original matrix and its inverse
##
## function cacheSolve is used to compute the inverse of the matrix and
## cache it the object that is passed
## 
## function t_testMatrixCache to test out the above two functions



## Function makeCacheMatrix accepts a matrix x and initializes the 
## internal matrix (x) with this matrix and 
## resets the matrix inverse (mi) to NULL
## It exposes apis
##      set and get to set and get the matrix x
##      setInverse and getInverse to set and get the matrix inverse (mi)

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(mInverse) mi <<- mInverse
    
    getInverse <- function() mi
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function cacheSolve accepts R Object x which was created using makeCacheMatrix function 
## It checks whether matrix inverse is available in R object x using function getInverse
## If the cache has a valid object (not NULL) it returns it to the caller function 
## If the cache is NULL, it gets the source matrix and computes the matrix inverse 
## using the solve function
## It then caches the matrix inverse returned by solve into x using setInverse
## finally it returns the matrix inverse to the caller

cacheSolve <- function(x, ...) {        
    mInverse <- x$getInverse()
    
    if (!is.null(mInverse)) {
        message("returning cached data")
        return (mInverse)
    }
    
    message("computing the matrix inverse")
    
    data <- x$get()
    mInverse <- solve(data, ...)
    x$setInverse(mInverse)
    
    mInverse
}


## Function t_testMatrixCache is used to test the matrix caching functions
## It accepts the size of the matrix (default value 5) and creates a square
## matrix using random uniform numbers
## It creates R object using makeCacheMatrix
## tests that the matrix invserse is computed for the first time and cached matrix
## is returned when executed for the second time
## It then creates a new matrix and sets it in the R object z
## It tests that the matrix inverse is recomputed for the new matrix
## It tests that the matrix is cached and returned when cacheSolve is invoked again

t_testMatrixCache <- function(msize = 5) {    
    numEle <- msize ^ 2
    mInput <- matrix(runif(numEle, min = 10, max = 750), nrow = msize, ncol = msize)
    
    print("Calling the cache function for the first time")
    z <- makeCacheMatrix(mInput)
    out <- cacheSolve(z)    
    print(out)
    
    print(" ")
    print("Calling the cache function for the second time")
    out <- cacheSolve(z)
    print(out)
    
    print(" ")
    print("Calling the set function to set a new matrix")
    mInput <- matrix(runif(numEle, min = 10, max = 750), nrow = msize, ncol = msize)
    z$set(mInput)
    
    print(" ")
    print("Calling the cache function for the first time after setting a new matrix")
    out <- cacheSolve(z)    
    print(out)
    
    print(" ")
    print("Calling the cache function for the second time")
    out <- cacheSolve(z)
    print(out)  
    
    print("Calling the cache function without any matrix")
    f <- makeCacheMatrix()
    out <- cacheSolve(f)    
    print(out)
    
    print(" ")
    print("Calling the set function to set a new matrix")
    mInput <- matrix(runif(numEle, min = 10, max = 750), nrow = msize, ncol = msize)
    f$set(mInput)
    
    print(" ")
    print("Calling the cache function for the first time after setting a new matrix")
    out <- cacheSolve(f)    
    print(out)
    
    print(" ")
    print("Calling the cache function for the second time")
    out <- cacheSolve(f)
    print(out)  
}

