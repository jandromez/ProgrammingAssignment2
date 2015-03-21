## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s<- NULL
        ## 1. set the value of the matrix
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        ## 2. get the value of the vector
        get <- function() x
        ## 3. set the value of the inverse
        setinverse <- function(inverse) s <<- inverse
        ## 4. get the value of the inverse
        getinverse <- function() s
        list(set=set, 
             get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        ## 'data' is assumed to be always invertible
        data <- x$get()
        ## if 'data' is a square invertible matrix, 
        ## then solve(X) returns its inverse.
        s <- solve(data) 
        x$setinverse(s)
        s
}
