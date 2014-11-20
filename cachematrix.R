## This piece of code is to cache the inverse of an invertible matrix, and avoid recomputing the inverse whenever possible. 
## This is done to save CPU cycles which otherwise would be incurred by repeated compuation of inverse of the matrix though the underlying matrix
## has not changed

## makeCacheMatrix takes a matrix (hopefully invertible) as its argument. By using four nested functions within it, it is desinged to be able to:

## (1) store away the given matrix (using the set function in it), 
## (2) or retrieve the stored matrix via. the get function defined in it. 
## (3) Similarly it can also store away the inverse of the matrix (via. function setInverse defined in it)
## (4) and retrieve the stored away inverse of the matrix via. the function getInverse

makeCacheMatrix <- function(x = matrix()) {
   xInverse <- NULL                        ## holds the inverse of the matrix
   
   set <- function(y) {                    ## this function stores the given matrix by overwriting the existing matrix in x
      x <<- y                              ## and resets its inverse to NULL 
      xInverse <<- NULL                     
   }
   get <- function()  x                    ## retrieve the stored away matrix and return it
   setInverse <- function(Inverse) xInverse <<- Inverse  ## store away the inverse of the matrix passed to the function -> setInverse
   getInverse <- function() xInverse      ## retreive the inverse of the matrix stored away by setInverse  
   list(set=set, get = get, setInverse = setInverse, getInverse = getInverse) ## return the list of functions which
                                                                              #can do the tasks described above so far.
}


## cacheSolve function returns the inverse of the matrix (supplied to it as an argument), either from the cache or
## after computing the inverse using Solve() function of R

cacheSolve <- function(x, ...) {  ## the argument supplied as input here should be the list returned by makeCacheMatrix function
        ## Return a matrix that is the inverse of 'x'
  
   xInverse <- x$getInverse()     # get the Inverse of the matrix
   if (!is.null(xInverse)) {      # if x$getInverse() does not return NULL then return the inverse returned from it
      message('getting cached data')
      return(xInverse)
   }
   
   matrix <- x$get()              # if x$getInverse() did return NULL then
   xInverse <- solve(matrix)      # compute the inverse and store it in xInverse
   x$setInverse(xInverse)         # pass the just computed xInverse to x$setInverse so that it stores it away in the cache
   xInverse                       # return the computed inverse of the matrix
}



