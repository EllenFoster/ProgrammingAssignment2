##Coursera: R Programming I -- Project Assignment #2

##Write a function that calculates and caches means so they can be 
##retrieved later, if needed.

#makeCacheMatrix creates a matrix so the inverse can be cached

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
