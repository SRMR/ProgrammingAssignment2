# Matrix inversion is usually a costly computation. If you have an application
# that uses the inverse of a matrix, it may be usefull to "cache" the result and
# use it if available rather than compute it repeatedly.
#
# The following two functions demonstates caching and using the inverse of a matrix.
#--------------------------------------------------------------------------------

# FUN#1: makeCacheMatrix creates a list of functions to
# 1. get the value of the matrix 
# 2. set the value of inverse of the matrix
# 3. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL

#
#     set <- function(y) {
#         x <<- y
#         inv <<- NULL
#     }
#
    get <- function() x                                  #retursn Matrix
    setinverse <- function(zz) minv <<- zz               #set inverse to cache
    getinverse <- function() minv                        #returns inverse
    list( getM=get, setI=setinverse, getI=getinverse)    #returns the list with functions 
}

# FUNC#2: Returns the inverse of the matrix. 
# Check if inverse exists in Cache. If exists, use that and skip the computation.
# If not, it computes the inverse to be used in further processing

cacheSolve <- function(yy, ...) {
    inv <- yy$getI()                          #get inverse 
    if(!is.null(inv)) {                       #check if inverse availble
        message("Using Matrix Inverse from Cache...")
        return(inv)                           #return, inverse and do not process any further 
    }
    
    matData <- yy$getM()     #get the matrix data
    inv <- solve(matData)    #Compute Matrix inverse
    yy$setI(inv)             #set the computed inverse to cache
    inv
}
