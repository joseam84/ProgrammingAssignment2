## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' 
    im <- x$getInvMatrix()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...) %*% data
    x$setInvMatrix(im)
    im
}
