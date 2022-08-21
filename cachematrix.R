## Below are two functions that are used to create an object that stores
## a matrix and cache's its inverse

## The first function, makeCacheMatric creates a special "matrix", 
## which is really a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse (mat)
## get the value of the inverse (mat)

makeCacheMatrix <- function(x = matrix()) {

  invmat <- NULL                                    # Create NULL invmat
  set <- function(y) {                              # A function that uses a dull
      x <<- y                                       # var to store the matrix and
      invmat <<- NULL                               # the state of invmat
  }
  get <- function() {                               # function to return matrix
      x
  }
  setinverse <- function(solve) {                   # function to set inverse of mat
      invmat <<- solve
  }
  getinverse <- function() {                        # function to get inverse of mat
      invmat
  }
  list(set = set, get = get,                        # list returned with above subsets
      setinverse = setinverse,
      getinverse = getinverse)
  }


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinverse()                          #get the inverse of the matrix
  if(!is.null(invmat)) {                            #if it's not null then return it
    message("getting cached data")
    return(invmat)
  }
  #If the function is not returned through the clause (FALSE) then proceed to invert the matrix
  data <- x$get()                                   #assign to data the cached (non-inverted) matrix
  invmat <- solve(data, ...)                        #solve (invert the matrix)
  x$setinverse(invmat)                              #set-back to list
  invmat                                            #return result
}


