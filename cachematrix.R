## makeCacheMatrix makes a list of functions,which can be used to
## manipulate the matrix, cacheSolve -- computes the inverse of the 
## matrix(if the inverse doesn't exist and the matrix is unchanged)
## else gives the cached value

## makeCacheMatrix--initializes the matrix,retrives the matrix,
## sets the inverse,gets the inverse

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        setMatrix <- function(y){
                x <<- y
                invm <<- NULL   
        }
        getMatrix <- function() x
        setInverse <- function(inv) invm <<- inv
        getInverse <- function() invm 
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
                setInverse = setInverse, getInverse = getInverse)
        
}


## solves for inverse if it doesn't exist else gives cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getInverse()
        b <- x$getMatrix()
        if(!is.null(invm) & identical(b,x$getMatrix)){
                message("Getting cached data")
                return(invm)
        }
        invm <- solve(b,...)
        x$setInverse(invm)
        invm
        
}
