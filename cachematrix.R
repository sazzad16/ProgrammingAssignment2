# Assignment: Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and their may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The pair
# of functions written here cache the inverse of a matrix.

# `makeCacheMatrix`: This function creates a special "matrix" object that can
# cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        # Change the matrix 'x'. The inverse matrix 'inv' is removed as it was,
        # if cached, not calculated from new 'x'.
        set <- function(newMatrix = matrix()) {
                inv <<- NULL
                # `set` will return something. It seems more logical to return
                # 'x' itself rather than a NULL.
                x <<- newMatrix
        }
        
        # Return the matrix 'x'.
        get <- function() x
        
        # Cache newly calculated inverse matrix.
        setSolve <- function(newInverse = matrix()) inv <<- newInverse
        
        # Return the cached inverse matrix.
        getSolve <- function() inv
        
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


# `cacheSolve`: This function computes the inverse of the special "matrix" 
# returned by `makeCacheMatrix` above. If the inverse has already been 
# calculated (and the matrix has not changed), then `cacheSolve` retrieves
# the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Fetch cached inverse matrix.
        inv <- x$getSolve()
        
        # Check if the inverse matrix was actually cached before.
        if(is.null(inv)) {
                # Inverse matrix is not cached.
                message('inverse matrix not cached')
                
                # Fetch the matrix.
                mat <- x$get()
                
                # Calculate inverse of that matrix using builtin R function.
                message('calculating inverse matrix')
                inv <- solve(mat)
                
                # Cache the inverse matrix for later use.
                x$setSolve(inv)
                
                message('inverse matrix calculated and cached')
        }
        else {
                message('cached inverse matrix found')
        }
        
        # Return the inverse matrix, whether it was cached before or
        # calculated just now.
        inv
}
