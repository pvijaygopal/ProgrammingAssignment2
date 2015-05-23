## Put comments here that give an overall description of what your
## functions do

## This is a two part function to retrieve cached data in case of complex, time consuming operations
## such as calculating the inverse of a matrix
## This program assumes that the inputted matrix is invertible; i.e. square non-singular matrix

## The first function receives the user input of the matrix, whose inverse is to be calculated.
## The function aims to return a list of matrix related values such as the set & get values for inputed matrix
## and the setinv& getinv values for the inverse of the matrix
## The second function physically solves for the inverse of the inputted matrix, but only after it has checked to see 
## if there is an already stored value for the inverse (in the case that the input matrix is unchanged)


## Write a short comment describing this function
## This function sets the inverse of the matrix initially at null. This allows to check if the inverse is already present 
## in the cache. The <<- operator registers the respective matrices in the parent environment, so that when the cachesolve
## function is called, the check for the previously calculated 'cached' matrix is possible 

makeCacheMatrix <- function(x = matrix()) 
        {
        inv <- NULL
        set <-function(matr)
        {
                x <<- mat
                inv <<- NULL
                
        }
        get <- function() 
                x                   ##gets the new matrix
        setinv <- function(solve)   ## calls function solve to calculate inverse
                inv <<- solve       ## assigns inv = returned inv value from solve function 
        getinv <- function()        ## gets the inverse if already present in cache
                inv
        list(set = set, get = get,setinv = setinv, getinv = getinv)  ##returns all updated values        

}


## Write a short comment describing this function
## This function is set to scope the parent environment or 'cache' 
## for an existent value of inverse for the unchanged matrix x (parsed through the first function)
## if inv is not null, i.e. contains a matrix, it indicates getting cached data and returns the inverse value
## In the event that x is a new matrix/changed matrix, it calculates the new inverse and 
## sets the new/updated value to the setinv value for the associated x matrix using x$setinv
## so if the new 'x' is called for inverse calculation, it will return the now newly calculated inverse from cache


cacheSolve <- function(x, ...)
        {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv() # getting data from cache
        if(!is.null(inv))
        {
                message("getting cached data")
                return(inv)
        }
        newmatrix <-x$get()
        inv <-solve(newmatrix, ...)
        x$setinv(inv)
}
