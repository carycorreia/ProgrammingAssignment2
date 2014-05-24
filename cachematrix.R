################################################################################
##
## Cousera:     Data Science Specialization
## Course:      R Programming
## Assignment:  Pragramming Assignment 2:  makeCacheMatrix() & cacheSolve()
## Student:     Cary Correia
## Date:        May 21, 2014
##
###############################################################################
##
##  makeCacheMatrix is a function that does the following:
##      accepts a matrix from a user and
##      creates a special "matrix" object 
##      that can cache its inverse
##############################################################################
##
makeCacheMatrix <- function(x = numeric(), n=integer(), c=integer()) {          ## declare 3 input matrix variables
        ## if an object is called without default values 
                m <- NULL                                                       ## set m flag to NULL
                set <- function(y, n, c) {                                                              
                x <<- matrix(y, n, c)                                           ## this will create the matrix using our inputs
                m <<- NULL
        }
                get <- function() x                                             ## create get function: so we can retrieve our matrix
                setinverse <- function(solve) m <<- solve                       ## create setinverse function:  for inverse calculation
                getinverse <- function() m                                      ## create getinverse function: for retrieving inverse matrix
                list(set = set, get = get,                                      ## create the list of 4 subfunctions
                     setinverse = setinverse,                                   
                     getinverse = getinverse)
##        
        ## at the end of this function there will be:
                ## 1) an empty input matrix
                ## 2) an empty numeric vector called m; set to NULL
                ## 3) 4 subfunctions that we can use operate on the special matrix
        ## to use type the following:
        ##       a <-makeCacheMatrix()  
        ##       a$set(c(your input numbers), nrow, ncol) 
        ##       for ex:
        ##       a$set(c(1,2,3,4),2,2); this will create a 2X2 matrix
}
##################################################################################
##
## cacheSolve is a function that does the following:
##      computes the inverse of the special "matrix" returned by makeÇacheMatrix()
##      If the inverse has already been calculated (and the matrix has not changed),
##      then cacheSolve will retrieve the inverse from the cache.
##################################################################################
##
cacheSolve <- function(x, ...) {                                                ## declare our input matrix for inversion
        m <- x$getinverse()                                                     ## assign the inverse to m
        if(!is.null(m)) {                                                       ## if m is not null then
                message("getting cached data")                                  ##         tell user we will use the "cached value"
                return(m)                                                       ##         return the cached inverse matrix
        }
        data <- x$get()                                                         ## else:  assign the input matrix to data
        m <- solve(data)                                                        ##        calculate it's inverse
        x$setinverse(m)                                                         ##        store its value in m
        m                                                                       ##        display the inverse matrix
        ## at the end of this function we should see two outcomes:
        ##      1) the inverse matrix should display (1st time)
        ##      2) msg "getting cached data" folled by inverse matrix
        ##
        ## note:  to prove that the inverse is truly returned 
        ##        simply multiply the input matrix %*% inverse matrix
        ##        you should see the identity matrix 
}
##