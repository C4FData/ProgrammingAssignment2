## Put comments here that give an overall description of what your
## functions do

## This function will define a number of subfunctions that will be used to cache a matrix

makeCacheMatrix <- function(x = matrix()) {
        ## initialize the object m to NULL
        m <- NULL
        
        ## define the subfunction set()
        ## this will take a matrix argument y and assign it to x
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        ## define the subfunction get()
        ## this will return the value of x
        get <- function() x
        
        ## define the subfunction setInverse()
        ## this will store the inverse of the matrix in cache
        setInverse <- function(solve) m <<- solve

        ## define the subfunction getInverse()
        ## this will retrieve the value of the inverse from cache
        getInverse <- function() m
        
        ## return a list of subfunctions for this function
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function will return the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## call the getInverse() function of the object x and return its contents
        m <- x$getInverse()
        
        ## if the contents of m are not empty (i.e. not NULL), then return the stored inverse
        if(!is.null(m)){
                # exit the function with the inverse of the matrix
                return(m)
        }
        
        ## the inverse of the matrix has not yet been calculated, so get the matrix
        ## using the get() subfunction of the x object
        data <- x$get()
        
        ## use the solve() function to calculate the inverse of the matrix and assign it to m
        m <- solve(data,...)
        
        ## store the inverse of the matrix using the setInverse() subfunction of the object x
        ## this will allow for faster returns the next time the function is called for the 
        ## same matrix
        x$setInverse(m)
        
        ## return the value of the inverse matrix
        m
}
