## ---------------------INTRODUCTION------------------------------
## Coursera - R Programming - Assignment 2
## This file created by Travis E. Lipstein, AUG 2015
##
## ---------------------REFERENCES-------------------------------
## 1. Peng R. D. (2015). Coursera - R Programing - Programming 
##    Assignment 2: Lexical Scoping.  Retrieved from 
##    <https://class.coursera.org/rprog-031/human_grading/view/
##      courses/975105/assessments/3/submissions>
## 2. Hadow, R (19AUG2015). R Programming - Programming Assignment 2: 
##    Lexical Scoping - Discussion Forums - Thread ID:579.
##
## ---------------------FUNCTION DESCRIPTION---------------------
## This function accepts a square invertible matrix and computes a 
## list of four functions:
## 1.) set the value of matrix
## 2.) get the value of the matrix
## 3.) set the value of the matrix inverse
## 4.) get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #Sets the value of x and m in environment
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #Retreives, sets values in cache
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}
##
## ----------------------FUNCTION DESCRIPTION-------------------
## The cacheSolve function checks to see if the calculation to
## invert the matrix has already been performed.  If it has, the
## computation is skipped and it returns the value stored in the
## environment cache.
cacheSolve <- function(x, ...) {
    #Attempts to retrieve stored values for matrix inverse
    m <- x$getsolve()
    #Checks to see if anything matrix was stored
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #Computes the matrix inverse if not already done so
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m #Returns the matrix inverse
}
