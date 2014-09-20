## makeCacheMatrix will create a special object
## that will cache its inverse when created
## cacheSolve will return the inverse of the matrix
## if the matrix has not changed, it will return the cached inverse

## m<-matrix(sample(1:40,36),nrow=6,ncol=6)

## makeCacheMatrix will create a special object
## the object will cache the inverse of a matrix given as input
## and will save it in cache by assigning this inverse to a variable
## in another environment

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## cacheSolve will return the inverse of the object's matrix
## if the matrix has not changed the inverse will be obtained 
## from the variable that already holds this in another environment

cacheSolve <- function(x = matrix(), ...) {
    m <- x$getmatrix()
    if( !is.null(m) ){
        message("Now loading cached data...")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}