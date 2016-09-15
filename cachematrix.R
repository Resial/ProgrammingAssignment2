# The function makeCacheMatrix follows the structure from the one on the assignment instructions.
## 

## This function assigns value z to function x, thus creating a special object that cache a matrix inverse. Variable MInv means matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
	MInv <- NULL
    set <- function (z) { x <<- z
        MInv <<-NULL }
        
    get <- function() x
    setMInv <- function(inverse) Minv <<- inverse
    getMInv <- function() Minv
    
    list (set = set, get = get,
          setMInv = setMInv,
          getMInv = getMInv)

}


## This function compute the inverse of the special matrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        MInv <- x$getMInv ()
        if(!is.null(MInv)) {
        	message("getting cached data")
        	return(MInv)
        }
        data <- x$get()
        MInv <- solve(data,...)
        x$setMInv(MInv)
        MInv
}
