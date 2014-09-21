# This first function, makeCacheMatrix, creates a special matrix object that can cache
# its inverse. Object is really a list of functions that
# 1) set the value of the matrix,
# 2) get the value of the matrix,
# 3) set the value of the inverse
# 4) get the value of the inverse

makeCacheMatrix <- function(x=matrix()) {
    inv <- matrix() # set matrix to null
    
    set <- function (y) { #changes value of x stored in vector, nulls out stored value of inv
        x <<- y
        inv <<- matrix()
    }
    
    get <- function() {x} #returns value of argument of makeCacheMatrix
    
    setinverse <- function(inverse) {inv <<- inverse} #sets inv in makeCacheMatrix to inverse
    #calculated in cacheSolve; superassignment operator
    # makes it available outside of this function
    # and in the parent environment (makeCacheMatrix)
    getinverse <- function() inv #returns value of inv from makeCacheMatrix
    
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    #makes functions accessible through subset
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function (x, ...) {
    inv <- x$getinverse() #gets value of inverse stored in makeCacheMatrix
    
    if (!is.na(inv[1,1])) {
        message("getting cached data")
        return(inv)
    } # if has not been calculated previously, will be null and will continue with else part of if statement
    # if inverse has been calculated, will give message and return previously calculated value
    
    data <- x$get() # if matrix needs to be calculated, this will load matrix from makeCacheMatrix into data
    #variable
    
    inv <- solve (data, ...) #calculates inverse of matrix
    
    x$setinverse(inv) #caches (saves) value of calculated matrix
    
    inv  # returns calculated inverse
}

