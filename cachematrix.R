## In this project I've created a set of two functions
## that cache the inverse of a matrix and store it in
## a different environment so that it could access to it
## at any moment.

## This function makes a list of three functions that are going
## to be used further in the project to retrieve the value of
## the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL ## In this line, we "clear" the variable
                    ## we are going to use later as an input
  
  set <- function(y){ ## We are just confirming the "reset"
    x <<- y           ## of the values of the function
    invmatrix <<- NULL
  }
  
  get <- function() {x} ## We are using this to ge the
                        ## original matrix we are supposed
                        ## to ber working with
  
  setinv <- function(inversa)
    {invmatrix <<- inversa} ## At this moment we are introducing
                            ## the inverse of the original matrix
                            ## in the new variable called "invmatrix"
  
  getinv <- function(){invmatrix} ## It just return the inverse previously
                                  ## calculated
  
  list(get=get,setinv=setinv,getinv=getinv)
}


## This function takes the list previously created with makeCacheMatrix
## and uses the functions we have stored in the list to obtain the value
## of the inverse matrix if it was not already stored in other
## environment.

cacheSolve <- function(x, ...) {
  m <- x$getinv() ## using the third formula in the list it obtains
                  ## the inverse matrix
  
  if (!is.null(m)){  ## Whether it is already store in another environment
                     ## it takes that result and prints it in the console...
    
    message("getting cached data")
    return(m)
    
  }
  data <- x$get()   ## However, if it's not solved, it proceeds to obtain
  m <- solve(data)  ## the inverse matrix of the data we have given
  x$setinv(m)       ## and stores it in another environment
  m                 ## and prints the result
}
