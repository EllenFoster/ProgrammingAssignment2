##Coursera: R Programming I -- Project Assignment #2

#makeCacheMatrix creates a matrix and caches the inverse

makeCacheMatrix <- function(x = matrix()) {
              y <- NULL
         
              create <- function(z) {
                x <<- z
                y <<- NULL
              }
         
              get <- function() x
              createinv <- function(inv) y <<- inv
              getinv <- function() y
              list(create = create, get = get, createinv = createinv, getinv = getinv)

}

#Return the inverse of the matrix created above

cacheSolve <- function(x, ...) {
        InvMatrix <- x$getinv()
        
        if(!is.null(InvMatrix)) {
                message("getting cached data")
                return(InvMatrix)
        }
        
        data <- x$get()
        InvMatrix <- solve(data, ...)
        x$createinv(InvMatrix)
        InvMatrix
}
