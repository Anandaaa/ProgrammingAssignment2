## Here we create special "matrices" for which the inverse can be stored
##

## This function creates the "matrix"
## Its actually a list containing functions to
## (1) set the entries of the matrix
## (2) get the entries of the matrix
## (3) set the entries of the inverse of the matrix
## (4) get the entries of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL    # inv will be the inverse of x, we initialize with NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(solved) inv <<- solved
        get_inverse <- function() inv
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function returns the inverse of the "matrix"
## It first checks whether an inverse already has been calculated
## If this is the case the stored inverse is returned
## Otherwise the inverse will be calculated, stored and returned

cacheSolve <- function(x, ...) {
        inv <- x$get_inverse()
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inverse(inv)
        inv
}
