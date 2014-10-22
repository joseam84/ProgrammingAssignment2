## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix works as an object to store a matrix and its 
## calculated inverted matrix.
## cacheSolve accept a cached matrix object and compute and cache 
## the inverted matrix into the object and return the computed result.


## Write a short comment describing this function

## function: makeCacheMatrix accept a matrix and store its inverse matrix.
##    $get : return the same matrix
##    $set : set the matrix and reset the inverted matrix to NULL
##    $getInvMatrix: get the calculated inverted matrix if cached, else 
##                   return NULL.
##    $setInvMatrix: set the calculated inverted matrix.
## param: x is a matrix, default to an [1,1] empty matrix
# l
makeCacheMatrix <- function(x = matrix()) {
    ##Getter for the matrix
    inv.matrix = NULL
    get <- function() x
    ##Setter for the matrix. if Changed set the inverted matrix to NULL
    set <- function(y){
        x <<- y
        inv.matrix <<- NULL
    }
    #Setter and getter for the inverted matrix
    getInvMatrix <- function() inv.matrix
    setInvMatrix <- function(im) inv.matrix <<- im 
    list(set = set, get = get,
         getInvMatrix = getInvMatrix,
         setInvMatrix = setInvMatrix)
}


## Write a short comment describing this function
## 
## function: cacheSolve accept the special cached matrix object and compute the
##            inverted matrix and cache the result. if the inverted matrix has
##            being calculated already, return it from the cache, else 
##            calculates the inverted matrix using the solve function, caches 
##            the result and return  the result.
##  
cacheSolve <- function(x, ...) {
    #if the inverted matrix has being calculated already, return it from the cache.
    im <- x$getInvMatrix()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    ## else calculate the inverted matrix using solve function
    ## Stores the result and return.
    data <- x$get()
    im <- solve(data, ...)
    x$setInvMatrix(im)
    ## Return a matrix that is the inverse of 'x' 
    im
}
