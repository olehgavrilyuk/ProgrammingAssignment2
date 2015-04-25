## This pair of functions caches inverse of the matrix and helps us not to waist time on recalulating
## inverse every time but to get cached inversed matrix if it was calculated previously. This helps alot in
## decreasing time needed for calculations.
## We use two functions: one to create special matrix object that can keep calculated inversed matrix in addition to original,
## second function caches (sets) inversed matrix into special matrix object thus giving possibility to use it in the future.

## This function creates special matrix object that keeps matrix and can also store inversed matrix for future use.
makeCacheMatrix <- function(x = matrix()) {     
        inverse <- NULL ## lets create var that would be holding our inversed matrix
        set <- function(y) { ## set var is actually a function that will take our original (not inversed) matrix from passed 'y' var and store it in x variable
                x <<- y ## store passed matrix in x variable
                inverse <<- NULL ## reset inversed matrix in case it was set previously, notice that we reset var beyond current function scope by using <<-
        }
        get <- function() x ## get var is actually a function that will return our original (not inversed) matrix
        setinverse <- function(solve) inverse <<- solve ## setinverse is the function that will set inversed matrix to be stored in our 'inverse' var
        getinverse <- function() inverse ## getinverse var is actually a function that will return our inversed matrix
        list(set = set, get = get, ## this list will assign predefined functions to appropriate names thus giving us possibility to call it externally by names
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function checks passed object (passed object x must be created using 'makeCacheMatrix" function above) to have cached inverse matrix.
## If there is - we return it saving time, if no - we calculate inverse matrix, cache it to passed object and return it.
cacheSolve <- function(x, ...) {
        ## lets check if passed object is actually created using makeCacheMatrix function, otherwise we are unable to process it
        ## we will check appropriate names of functions to be present in passed list names
        if (sum(c("get","set","setinverse","getinverse") %in% names(x)) != 4) {
                stop("wrong object (x), use makeCacheMatrix to create appropriate object to be processed by this function")
        }
        inverse <- x$getinverse() ## we get inverse matrix from passed object and assign to our 'inverse' var
        if(!is.null(inverse)) { ## if the var is not null then we return it as inverse matrix was cached previously (and assigned to inverse above) and we don't need to recalculate it
                message("getting cached data") ## lets inform user that we have it in cache
                return(inverse) ## and return it
        }
        ## if cached inverse matrix is null then we must recalulate it
        message("calculating and caching inverse matrix") ## lets inform user about it
        data <- x$get() ## lets get original (not inversed) matrix from object
        inverse <- solve(data, ...) ## lets solve it producing inversed matrix
        x$setinverse(inverse) ## set inversed matrix to passed object's vars for future use
        inverse ##return it
}
