## Write a short comment describing this function
## This function will cahe the dada of a matrix

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL # seting NULL value to the inv variable
          set <- function(y){ # setting the value of the matrix
                    x <<- y # capable of modifying variables
                    inv <<- NULL 
          }
          get <- function() {x}
          setInverse <- function(inverse) {inv <<- inverse} # setting the value of the inverse
          
          getInverse <- function() {inv} # Getting the value of the inverse
          list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to set cached data!
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
              message("Getting Cached Data!")
              return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

M1 <- matrix(1:4, nrow = 2, ncol = 2) # Creating a 2 x 2 matrix
M1C <- makeCacheMatrix(M1) # Caching the matrix M1 into M1C
M1C$get() ## Getting the cached data
M1C$getInverse() # Getting inverted matrix
cacheSolve(M1C) 

