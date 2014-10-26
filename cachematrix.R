## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## There are two fuctions - makeCacheMatrix does at list of functions to be used
# in the second function cacheSolver() where the inverse of a matrix is done
# and stored in the variable "m". The variable m is located in the makeCacheMatrix()
# enviroment with the functions get , set , setinverse , getinverse
# the <<- operation allows to stored/acess the inverse value at the cachesolve()
# Also  it is used in the function Set() to set the m value to NULL, so that 
# getinverse() would return true, then recalculating the inverse matrix when we used
# cacheSolve function

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL #set m as a variable in makeCacheMatrix()
        set <- function(y) {
                x <<- y # Give a new matrix to solve
                m <<- NULL # reset value from the inverse matrix to NULL
        }                  # Otherwise the value of getinverse() may not be NULL
        get <- function() x # Get actual matrix loaded to be used in cachesolve
        setinverse <- function(solve) m <<- solve # Give a predefined value
        getinverse <- function() m #Get inverse matrix value or NULL
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) # List to be used for CacheSolve function
        
}


## Write a short comment describing this function
# Make use of the m value stored in makeCacheMatrix environment
# since the functions stored in x (produce with makeCacheMatrix()) are also
# in makeCacheMatrix environment they will get the value m for stored the value
# or to call the variable. Look that the value m is used with <<- operator at 
# makeCacheMatrix()


cacheSolve <- function(x , ...) 
        {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() # first time X is call or set() was used to modify 
                            # x and m variable in makeCacheMatrix environment
        if(!is.null(m)) {
                message("getting cached data") ## print message in the console
                return(m) # Return inverse matrix calculated, faster than recalculated
        }
        data <- x$get() # Retrieve the x matrix to calculated his inverse
        m <- solve(data, ...) # calculate the inverse of x matrix
        x$setinverse(m) # stored the inverse matrix at m in the makeCacheMatrix environment
        m
}


