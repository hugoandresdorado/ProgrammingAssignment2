##R Programming  Assignment 2
##By Hugo Andres Dorado B.

## This function creates a special "matrix" object that can cache its 
## inverse



makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    #a function to set the  matrix and declare the variable inv
    set <- function(y){
        x <<- y
        inv <<- NULL     
    }
    #a function to get the matrix
    get     <- function()x 
    #a function to set the inverse 
    setInverse  <- function(inverse) inv <<- inverse
    #a function to get inverse 
    getInverse  <- function()inv
    
    # all function will be set in a list
    list(set=set, get = get,
         setInverse = setInverse,       
         getInverse = getInverse)
}


##This function computes the inverse of the special "matrix" returned by
##makeCacheMatrix above. If the inverse has already been calculated (and 
#the matrix has not changed), then cacheSolve should retrieve the inverse 
##from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## The inverse is obtained, could be null if is the first time that we run 
    inv <- x$getInverse()
    ## If we have gotten the inverse before, we would have the value in this step
    if(!is.null(inv)){
        message("getting cached matrix")
        return(inv)
    }
    ## If no, the function will compute the inverse below
    matrix <- x$get()
    inv  <- solve(matrix,...)
    x$setInverse(inv)
    inv
}

#Examples

mat1 <- matrix(c(1,2,3,4),2,2)
set.seed(123)
mat2 <- matrix(runif(25),5,5)

mkCchMat1 <- makeCacheMatrix(mat1)
mkCchMat2 <-  makeCacheMatrix(mat2)

cacheSolve(mkCchMat1)
cacheSolve(mkCchMat2)
