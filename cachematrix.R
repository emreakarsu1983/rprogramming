## Assignment for Week 3; 21 Aug 2016; GitHub user: emreakarsu1983

## Function Desc: creates a "matrix" object that caches its l_inverse

CacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
    l_inv <- NULL                             ## initialize l_inv as NULL; will hold value of matrix l_inverse 
    set <- function(y) {                    ## define the set function to assign new 
        x <<- y                             ## value of matrix in parent environment
        l_inv <<- NULL                        ## if there is a new matrix, reset l_inv to NULL
    }
    get <- function() x                     ## define the get fucntion - returns value of the matrix argument
    
    setl_inverse <- function(l_inverse) l_inv <<- l_inverse  ## assigns value of l_inv in parent environment
    getl_inverse <- function() l_inv                     ## gets the value of l_inv where called
    list(set = set, get = get, setl_inverse = setl_inverse, getl_inverse = getl_inverse)  ## you need this in order to refer 
                                                                                  ## to the functions with the $ operator
}


## function : Compute the l_inverse of the "matrix" returned by CacheMatrix above.
## If the l_inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the l_inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the l_inverse of 'x'
    l_inv <- x$getl_inverse()
    if(!is.null(l_inv)) {
        message("getting cached data")
        return(l_inv)
    }
    data <- x$get()
    l_inv <- solve(data, ...)
    x$setl_inverse(l_inv)
    l_inv
}


