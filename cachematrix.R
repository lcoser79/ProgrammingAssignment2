# Second programming assignment
# Lucas Coser

# 'makeCacheMatrix' function
# Input arguments: none
# How to use it:
# 1. Use 'makeCacheMatrix()' to create a cache for a matrix and its inverse.
# 2. Set the matrix using 'set(matrix)'.
# 3. Retrieve the matrix using 'get()'.
# 4. Set the inverse matrix using 'setinverse(inverse_matrix)'.
# 5. Retrieve the inverse matrix using 'getinverse()'.
makeCacheMatrix <- function() {
        
        # Initializes a variable 'cache' to store the matrix.
        cache <- NULL

        # The function 'set' is defined within 'makeCacheMatrix'. It takes one
        # argument 'matrix', representing a matrix. Inside the function, it
        # assigns the input matrix 'matrix' to the variable 'cache' using '<<-',
        # and it also resets the inverse to NULL
        set <- function(matrix) {
                cache <<- matrix
                inverse <<- NULL
        }
        
        # The function 'get' returns the current value of 'cache'
        get <- function() {
                cache
        }

        # The function 'setinverse' takes one argument 'inverse_matrix', which
        # represents the inverse of the matrix. Inside the function, it assigns
        # the input inverse matrix to the variable 'inverse' using the operator
        # '<<-'
        setinverse <- function(inverse_matrix) {
                inverse <<- inverse_matrix
        }
        
        # The function 'getinverse' returns the current value of 'inverse'

        getinverse <- function() {
                inverse
        }
        
        #. Creates a list containing the four functions defined above, i.e.,
        # 'set', 'get', 'setinverse', and 'getinverse'
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
makeCacheMatrix <- function() {
        
        # Initializes a variable 'cache' to store the matrix.
        cache <- NULL

        # The function 'set' is defined within 'makeCacheMatrix'. It takes one
        # argument 'matrix', representing a matrix. Inside the function, it
        # assigns the input matrix 'matrix' to the variable 'cache' using '<<-',
        # and it also resets the inverse to NULL
        set <- function(matrix) {
                cache <<- matrix
                inverse <<- NULL
        }
        
        # The function 'get' returns the current value of 'cache'
        get <- function() {
                cache
        }

        # The function 'setinverse' takes one argument 'inverse_matrix', which
        # represents the inverse of the matrix. Inside the function, it assigns
        # the input inverse matrix to the variable 'inverse' using the operator
        # '<<-'
        setinverse <- function(inverse_matrix) {
                inverse <<- inverse_matrix
        }
        
        # The function 'getinverse' returns the current value of 'inverse'

        getinverse <- function() {
                inverse
        }
        
        #. Creates a list containing the four functions defined above, i.e.,
        # 'set', 'get', 'setinverse', and 'getinverse'
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# ----------------------

# 'cacheSolve' function
# It takes at least one argument 'x', representing the cache matrix object.
# The '...' notation allows additionalarguments to be passed to the
# 'solve' function

cacheSolve <- function(x, ...) {

        # Retrieves the cached inverse matrix ('i') from the cache matrix object
        # 'x' using the 'getinverse' function defined in the 'makeCacheMatrix'
        # function
        i <- x$getinverse()

        # This conditional statement checks if the inverse matrix 'i' is not
        # NULL. If it's not NULL, it means that the inverse matrix is already
        # cached, and there's no need to recompute it. In that case, it prints
        # a message indicating that cached data is being retrieved and returns
        # the cached inverse matrix.
        if (!is.null(i)) {

                message("Getting cached data")
                
                return(i)
        }

        # If the inverse matrix is NULL, this line retrieves the original matrix
        # ('data') from the cache matrix object 'x' using the 'get' function
        data <- x$get()

        # Computes the inverse matrix 'i' using the 'solve' function applied to
        # the original matrix 'data'. The '...' notation allows additional
        # arguments to be passed to the 'solve' function
        i <- solve(data, ...)

        # After computing the inverse matrix 'i', caches the inverse matrix by
        # setting it in the cache matrix object 'x' using the 'setinverse'
        # function
        x$setinverse(i)

        # Finally, the computed inverse matrix 'i' is returned as the result
        # of the function
        i
}

# Tests
B <- matrix(c(1, -1, 7, -8), 2, 2)
B1 <- makeCacheMatrix()
B1$set(B)

inverse_B <- B1$getinverse()
B1$setinverse(inverse_B)
