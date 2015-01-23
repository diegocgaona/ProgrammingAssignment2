## This functions use a cache to optimize the solve
## of a inverse matrix without computate all again.

## This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      
      i <- NULL # where we gone to store the inverse matrix
      
      set <- function(y) { ## Set the vector x to the value passed in.
            x <<- y ## used <<- to assign x in higher environment
            i <<- NULL ## to reset m to NULL
      }
      
      get <- function() x   ## return the value of x stored in the
      ## makeCacheMatrix object
      
      setimatrix <- function(imatrix) i <<- imatrix ## set the value of i
      getimatrix <- function() i
      
      list(set = set,       ## creates list of nested functions with tags
           get = get,       
           setimatrix = setimatrix,
           getimatrix = getimatrix)
}

## This function will read a cached inverse matrix if it already
## exists, or calculate if it don't.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getimatrix() ## Use getimatrix() function in makeCacheMatrix
      
      if(!is.null(i)) {  ## If i is not NULL, gets cached inverse matrix
            
            message("getting cached data")
            return(i)      ## return cached inverse matrix
      }
      
      data <- x$get()
      i <- solve(data, ...)  ## calculate the inverse matrix of data passing 
      ## in any additional parameters received.
      
      x$setimatrix(i)   ## cache the newly caclulated inverse matrix 
                        ## in makeVector
      
      i                 ## return newly calculated inverse matrix
}


## Bellow one example of how to use the functions:
## > testmatrix <- matrix(data = sample(1:25, 25),nrow = 5,ncol = 5)  # Creates a Matrix
## > matrix1 <- makeCacheMatrix(testmatrix) # Create the special matrix
## > cacheSolve(x = matrix1) # Solve the inverse matrix or get the result from cache
